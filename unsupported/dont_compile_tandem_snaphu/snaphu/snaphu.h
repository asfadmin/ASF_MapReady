/* file snaphu.h */



/**********************/
/* defined constants  */
/**********************/

#define PROGRAMNAME          "snaphu"
#define VERSION              "0.91a"
#define PI                   3.14159265358979
#define TWOPI                6.28318530717959
#define SQRTHALF             0.70710678118655
#define MAXSTRLEN            512
#define MAXLINELEN           2048
#define TRUE                 1
#define FALSE                0
#define COLWISEINT           TRUE
#define LARGESHORT           32000
#define LARGELONG            2000000000
#define LARGELONGLONG        9000000000000000000
#define LARGEFLOAT           1.0e35
#define VERYFAR              LARGELONG
#define GROUNDROW            -2
#define GROUNDCOL            -2
#define MAXGROUPBASE         LARGELONG
#define ONTREE               1
#define INBUCKET             2
#define NOTINBUCKET          3
#define POSINCR              0
#define NEGINCR              1
#define NOCOSTSHELF          (-LARGESHORT)
#define MINSCALARCOST        1
#define INITARRSIZE          500
#define NEWNODEBAGSTEP       500
#define CANDIDATEBAGSTEP     500
#define NEGBUCKETFRACTION    1.0
#define POSBUCKETFRACTION    1.0
#define CLIPFACTOR           0.6666666667
#define DEF_OUTFILE          "snaphu.out"
#define DEF_SYSCONFFILE      ""     /* "/usr/local/snaphu/snaphu.conf" */
#define DEF_WEIGHTFILE       ""     /* "snaphu.weight" */
#define DEF_AMPFILE          ""     /* "snaphu.amp" */
#define DEF_AMPFILE2         ""     /* "snaphu.amp" */
#define DEF_MAGFILE          ""     /* "snaphu.mag" */
#define DEF_CORRFILE         ""     /* "snaphu.corr" */
#define DEF_ESTFILE          ""     /* "snaphu.est" */
#define MAXITERATION         5000
#define NEGSHORTRANGE        SHRT_MIN
#define POSSHORTRANGE        SHRT_MAX
#define MAXRES               SCHAR_MAX
#define MINRES               SCHAR_MIN
#define PROBCOSTP            -16
#define NULLFILE             "/dev/null"
#define DEF_ERRORSTREAM      stderr
#define DEF_OUTPUTSTREAM     stdout
#define DEF_VERBOSESTREAM    NULL
#define DEF_INITONLY         FALSE
#define DEF_INITMETHOD       MSTINIT
#define DEF_UNWRAPPED        FALSE
#define DEF_EVAL             FALSE
#define DEF_WEIGHT           1
#define DEF_NOSTATCOSTS      FALSE
#define DEF_DEFO             FALSE
#define DEF_VERBOSE          FALSE
#define DEF_AMPLITUDE        TRUE
#define DEF_HAVEMAGNITUDE    FALSE
#define DEF_SETEDGES         FALSE
#define AUTOCALCSTATMAX      0
#define USEMAXCYCLEFRACTION  -123
#define COMPLEX_DATA         1         /* file format */
#define FLOAT_DATA           2         /* file format */
#define ALT_LINE_DATA        3         /* file format */
#define ALT_SAMPLE_DATA      4         /* file format */
#define ABNORMAL_EXIT        1         /* exit code */
#define NORMAL_EXIT          0         /* exit code */
#define DUMP_PATH            "/tmp/"   /* default location for writing dumps */
#define NARMS                8         /* number of arms for Despeckle() */
#define ARMLEN               5         /* length of arms for Despeckle() */
#define KEDGE                5         /* length of edge detection window */
#define ARCUBOUND            200       /* capacities for cs2 */
#define MSTINIT              1         /* initialization method */
#define MCFINIT              2         /* initialization method */
#define BIGGESTDZRHOMAX      10000.0


/* SAR and geometry parameter defaults */

#define DEF_BPERP            -150.0
#define DEF_NLOOKS           40
#define DEF_NLOOKSRANGE      1
#define DEF_NLOOKSAZ         5
#define DEF_NCORRLOOKSRANGE  3  
#define DEF_NCORRLOOKSAZ     15
#define DEF_THETA            (23.0*PI/180.0)
#define DEF_NOMRANGE         831000.0
#define DEF_DR               8.0
#define DEF_DA               20.0 
#define DEF_RANGERES         10.0
#define DEF_AZRES            6.0
#define DEF_LAMBDA           0.0565647


/* scattering model defaults */

#define DEF_KDS              0.02
#define DEF_N                8.0
#define DEF_DZRCRITFACTOR    2.0
#define DEF_SHADOW           FALSE
#define DEF_DZEIMIN          -4.0
#define DEF_LAYWIDTH         16 
#define DEF_LAYMINEI         1.25
#define DEF_SLOPERATIOFACTOR 1.18
#define DEF_SIGSQEI          100.0


/* decorrelation model parameters */

#define DEF_DRHO             0.005
#define DEF_RHOSCONST1       1.3
#define DEF_RHOSCONST2       0.14
#define DEF_CSTD1            0.4
#define DEF_CSTD2            0.35
#define DEF_CSTD3            0.06
#define DEF_DEFAULTCORR      0.01
#define DEF_RHOMINFACTOR     1.3


/* pdf model parameters */

#define DEF_DZLAYPEAK        -2.0
#define DEF_AZDZFACTOR       0.99
#define DEF_DZEIFACTOR       4.0 
#define DEF_DZLAYFACTOR      1.0
#define DEF_LAYCONST         0.9
#define DEF_LAYFALLOFFCONST  2.0
#define DEF_SIGSQSHORTMIN    1
#define DEF_SIGSQLAYFACTOR   0.1


/* deformation mode parameters */

#define DEF_DEFOAZDZFACTOR   1.0
#define DEF_DEFOTHRESHFACTOR 1.2
#define DEF_DEFOMAX          1.2
#define DEF_SIGSQCORR        0.05
#define DEF_DEFOLAYCONST     0.9


/* algorithm parameters */

#define DEF_MAXFLOW          4
#define DEF_KROWEI           65
#define DEF_KCOLEI           257
#define DEF_KPARDPSI         7
#define DEF_KPERPDPSI        7
#define DEF_THRESHOLD        0.001
#define DEF_INITDZR          2048.0
#define DEF_INITDZSTEP       100.0
#define DEF_MAXCOST          1000.0
#define DEF_COSTSCALE        100.0 
#define DEF_COSTSCALEBPERP   110.0 
#define DEF_SRCROW           -1
#define DEF_SRCCOL           -1
#define DEF_P                PROBCOSTP
#define DEF_NSHORTCYCLE      200
#define DEF_MAXNEWNODECONST  0.0008
#define DEF_MAXCYCLEFRACTION 0.00001
#define DEF_SOURCEMODE       0
#define DEF_MAXNFLOWCYCLES   USEMAXCYCLEFRACTION
#define DEF_INITMAXFLOW      9999
#define INITMAXCOSTINCR      200
#define NOSTATINITMAXFLOW    15
#define DEF_ARCMAXFLOWCONST  3
#define DEF_DUMPALL          FALSE
#define DUMP_INITFILE        "snaphu.init"
#define DUMP_FLOWFILE        "snaphu.flow"
#define DUMP_EIFILE          "snaphu.ei"
#define DUMP_ROWCOSTFILE     "snaphu.rowcost"
#define DUMP_COLCOSTFILE     "snaphu.colcost"
#define DUMP_MSTROWCOSTFILE  "snaphu.mstrowcost"
#define DUMP_MSTCOLCOSTFILE  "snaphu.mstcolcost"
#define DUMP_MSTCOSTSFILE    "snaphu.mstcosts"
#define DUMP_CORRDUMPFILE    "snaphu.corr"
#define INCRCOSTFILEPOS      "snaphu.incrcostpos"
#define INCRCOSTFILENEG      "snaphu.incrcostneg"
#define DEF_CS2SCALEFACTOR   8
#define DEF_FIRSTCOL         1
#define DEF_FIRSTROW         1
#define DEF_NTILECOL         0
#define DEF_NTILEROW         0
#define DEF_TILESIZEDEST     FALSE


/* default file formats */

#define DEF_INFILEFORMAT              COMPLEX_DATA
#define DEF_UNWRAPPEDINFILEFORMAT     ALT_LINE_DATA
#define DEF_MAGFILEFORMAT             FLOAT_DATA
#define DEF_OUTFILEFORMAT             ALT_LINE_DATA
#define DEF_CORRFILEFORMAT            ALT_LINE_DATA
#define DEF_ESTFILEFORMAT             ALT_LINE_DATA
#define DEF_AMPFILEFORMAT             ALT_SAMPLE_DATA

/* command-line usage help strings */

#define OPTIONSHELPFULL\
 "usage:  snaphu [options] infile linelength\n"\
 "options:\n"\
 "  -d              use deformation mode costs\n"\
 "  -f <filename>   read configuration parameters from file\n"\
 "  -o <filename>   write output to file\n"\
 "  -a <filename>   read amplitude data from file\n"\
 "  -A <filename>   read power data from file\n"\
 "  -m <filename>   read interferogram magnitude data from file\n"\
 "  -c <filename>   read correlation data from file\n"\
 "  -e <filename>   read coarse unwrapped-phase estimate from file\n"\
 "  -E <filename>   read tile-sized coarse unwrapped estimate from file\n"\
 "  -w <filename>   read scalar weights from file\n"\
 "  -b <decimal>    perpendicular baseline (meters)\n"\
 "  -p <decimal>    Lp-norm parameter p\n"\
 "  -i              do initialization and exit\n"\
 "  -n              do not use statistical costs (with -p or -i)\n"\
 "  -u              infile is already unwrapped; initialization not needed\n"\
 "  -s              set boundary conditions to those of unwrapped input\n"\
 "  -q              quantify cost of unwrapped infile and exit\n"\
 "  -v              give verbose output\n"\
 "  --mst           use MST algorithm for initialization\n"\
 "  --mcf           use MCF algorithm for initialization\n"\
 "  --aa <filename1> <filename2> read amplitude data from next two files\n"\
 "  --AA <filename1> <filename2> read power data from next two files\n"\
 "  --costinfile <filename>      read statistical cost input from file\n"\
 "  --costoutfile <filename>     write statistical cost output to file\n"\
 "  --tile x1 y1 nx ny           read only ny x nx patch beginning at x1 y1\n"\
 "  --debug, --dumpall           dump all intermediate data arrays\n"\
 "\n"

#define OPTIONSHELPBRIEF\
 "usage:  snaphu [options] infile linelength\n"\
 "most common options:\n"\
 "  -d              use deformation mode costs\n"\
 "  -f <filename>   read configuration parameters from file\n"\
 "  -o <filename>   write output to file\n"\
 "  -a <filename>   read amplitude data from file\n"\
 "  -c <filename>   read correlation data from file\n"\
 "  -b <decimal>    perpendicular baseline (meters)\n"\
 "  -i              do initialization and exit\n"\
 "  -v              give verbose output\n"\
 "  --mst           use MST algorithm for initialization\n"\
 "  --mcf           use MCF algorithm for initialization\n"\
 "  --help, -h      print a more complete list of options\n"\
 "\n"




/********************/
/* type definitions */
/********************/

typedef struct nodeST{
  short row,col;                /* row, col of this node */
  unsigned long level;          /* tree level */
  struct nodeST *next;          /* ptr to next node in thread or bucket */
  struct nodeST *prev;          /* ptr to previous node in thread or bucket */
  struct nodeST *pred;          /* parent node in tree */
  long group;                   /* for marking label */
  long incost,outcost;          /* costs to, from root of tree */
}nodeT;


typedef struct costST{
  short offset;                 /* offset of wrapped phase gradient from 0 */
  short sigsq;                  /* variance due to decorrelation */
  short dzmax;                  /* largest discontinuity on shelf */
  short laycost;                /* cost of layover discontinuity shelf */
}costT;


typedef struct candidateST{
  nodeT *from, *to;             /* endpoints of candidate arc */
  long violation;               /* magnitude of arc violation */
  short arcrow,arccol;          /* indexes into arc arrays */
  signed char arcdir;           /* direction of arc (1=fwd, -1=rev) */
}candidateT;


typedef struct bucketST{
  long size;                    /* number of buckets in list */
  long curr;                    /* current bucket index */
  long maxind;                  /* maximum bucket index */
  long minind;                  /* smallest (possibly negative) bucket index */
  nodeT **bucket;               /* array of first nodes in each bucket */
  nodeT **bucketbase;           /* real base of bucket array */
}bucketT;
  

typedef struct paramST{

  /* SAR and geometry parameters */
  double bperp;           /* nominal perpendiuclar baseline (meters) */
  long nlooks;            /* number of independent looks in correlation data */
  long nlooksrange;       /* number of looks in range for input data */ 
  long nlooksaz;          /* number of looks in azimuth for input data */ 
  long ncorrlooksrange;   /* number of looks in range for correlation */ 
  long ncorrlooksaz;      /* number of looks in azimuth for correlation */ 
  double theta;           /* look angle (radians) */
  double nomrange;        /* nominal slant range (meters) */
  double dr;              /* range bin spacing (meters) */
  double da;              /* azimuth bin spacing (meters) */
  double rangeres;        /* range resolution (meters) */
  double azres;           /* azimuth resolution (meters) */
  double lambda;          /* wavelength (meters) */

  /* scattering model parameters */
  double kds;             /* ratio of diffuse to specular scattering */
  double n;               /* power specular scattering component */
  double dzrcritfactor;   /* fudge factor for linearizing scattering model */
  signed char shadow;     /* allow discontinuities from shadowing */
  double dzeimin;         /* lower limit for backslopes (if shadow = FALSE) */
  long laywidth;          /* width of window for summing layover brightness */
  double layminei;        /* threshold brightness for assuming layover */
  double sloperatiofactor;/* fudge factor for linearized scattering slopes */
  double sigsqei;         /* variance (dz, meters) due to uncertainty in EI */

  /* decorrelation model parameters */
  double drho;            /* step size of correlation-slope lookup table */
  double rhosconst1,rhosconst2;/* for calculating rho0 in biased rho */
  double cstd1,cstd2,cstd3;/* for calculating correlation power given nlooks */
  double defaultcorr;     /* default correlation if no correlation file */
  double rhominfactor;    /* threshold for setting unbiased correlation to 0 */

  /* pdf model parameters */
  double dzlaypeak;       /* range pdf peak for no discontinuity when bright */
  double azdzfactor;      /* fraction of dz in azimuth vs. rnage */
  double dzeifactor;      /* nonlayover dz scale factor */
  double dzlayfactor;     /* layover regime dz scale factor */
  double layconst;        /* normalized constant pdf of layover edge */
  double layfalloffconst; /* factor of sigsq for layover cost increase */
  long sigsqshortmin;     /* min short value for costT variance */
  double sigsqlayfactor;  /* fration of ambiguityheight^2 for layover sigma */

  /* deformation mode parameters */
  double defoazdzfactor;  /* scale for azimuth ledge in defo cost function */
  double defothreshfactor;/* factor of rho0 for discontinuity threshold */
  double defomax;         /* max discontinuity (cycles) from deformation */
  double sigsqcorr;       /* variance in measured correlation */
  double defolayconst;    /* layconst for deformation mode */

  /* algorithm parameters */
  signed char eval;       /* evaluate unwrapped input file if TRUE */
  signed char unwrapped;  /* input file is unwrapped if TRUE */
  signed char initonly;   /* exit after initialization if TRUE */
  signed char initmethod; /* MST or MCF initialization */
  signed char nostatcosts;/* don't uses statistical costs if TRUE */
  signed char defo;       /* deformation mode */
  signed char dumpall;    /* dump intermediate files */
  signed char verbose;    /* print verbose output */
  signed char amplitude;  /* intensity data is amplitude, not power */
  signed char havemagnitude; /* flag to create correlation from other inputs */
  signed char setedges;   /* fix boundary conditions of unwrapped input */
  long initmaxflow;       /* maximum flow for initialization */
  long arcmaxflowconst;   /* units of flow past dzmax to use for initmaxflow */
  long maxflow;           /* max flow for tree solve looping */
  long krowei, kcolei;    /* size of boxcar averaging window for mean ei */
  long kpardpsi;          /* length of boxcar for mean wrapped gradient */
  long kperpdpsi;         /* width of boxcar for mean wrapped gradient */
  double threshold;       /* thershold for numerical dzrcrit calculation */
  double initdzr;         /* initial dzr for numerical dzrcrit calc. (m) */
  double initdzstep;      /* initial stepsize for spatial decor slope calc. */
  double maxcost;         /* min and max float values for cost arrays */
  double costscale;       /* scale factor for discretizing to integer costs */
  double costscalebperp;  /* bperp for auto normalizing costscale to 100 */
  long srcrow,srccol;     /* source node location */
  double p;               /* power for Lp-norm solution (less than 0 is MAP) */
  long nshortcycle;       /* number of points for one cycle in short int dz */
  double maxnewnodeconst; /* number of nodes added to tree on each iteration */
  long maxnflowcycles;    /* max number of cycles to consider nflow done */
  double maxcyclefraction;/* ratio of max cycles to pixels */
  long sourcemode;        /* 0, -1, or 1, determines how tree root is chosen */
  long cs2scalefactor;    /* scale factor for cs2 initialization (eg, 3-30) */
  long firstcol;          /* first column of tile to process (index from 1) */
  long ntilecol;          /* number of columns in tile to process */
  long firstrow;          /* first row of tile to process (index from 1) */
  long ntilerow;          /* number of rows in tile to process */
  signed char tilesizedest; /* TRUE if unwrapped estimate is size of tile */
  
  /* run option parameters */
  char *costinfile;       /* file to read cost data from */
  char *costoutfile;      /* file to write cost data to */

  /* file format parameters */
  signed char infileformat;           /* input file format */
  signed char unwrappedinfileformat;  /* input file format if unwrapped */
  signed char magfileformat;          /* interferogram magnitude file format */
  signed char outfileformat;          /* output file format */
  signed char corrfileformat;         /* correlation file format */
  signed char weightfileformat;       /* weight file format */
  signed char ampfileformat;          /* amplitude file format */
  signed char estfileformat;          /* unwrapped-estimate file format */

  /* dump file names */
  char *initfile;                     /* unwrapped initialization */
  char *flowfile;                     /* flows of unwrapped solution */
  char *eifile;                       /* despckled, normalized intensity */
  char *rowcostfile;                  /* statistical azimuth cost array */
  char *colcostfile;                  /* statistical range cost array */
  char *mstrowcostfile;               /* scalar initialization azimuth costs */
  char *mstcolcostfile;               /* scalar initialization range costs */
  char *mstcostsfile;                 /* scalar initialization costs (all) */
  char *corrdumpfile;                 /* correlation coefficient magnitude */

}paramT;




/***********************/
/* function prototypes */
/***********************/


/* functions in snaphu_solver.c */

long TreeSolve(nodeT **nodes, nodeT *ground, nodeT *source, 
	       candidateT **candidatelistptr, 
	       candidateT **candidatebagptr, long *candidatelistsizeptr,
	       long *candidatebagsizeptr, bucketT *bkts, short **flows, 
	       void **costs, short ***incrcosts, nodeT ***apexes, 
	       signed char **iscandidate, long ngroundarcs, long nflow, 
	       float **mag, float **wrappedphase, char *outfile, 
	       long nrow, long ncol, paramT *params);
void AddNewNode(nodeT *from, nodeT *to, long arcdir, bucketT *bkts, 
		long nflow, short ***incrcosts, long arcrow, long arccol, 
		long nrow, paramT *params);
void CheckArcReducedCost(nodeT *from, nodeT *to, nodeT *apex, 
			 long arcrow, long arccol, long arcdir, 
			 long nflow, nodeT **nodes, nodeT *ground, 
			 candidateT **candidatebagptr, 
			 long *candidatebagnextptr, 
			 long *candidatebagsizeptr, void **costs, 
			 short ***incrcosts, signed char **iscandidate, 
			 long nrow, long ncol, paramT *params);
void InitTree(nodeT *source, nodeT **nodes, nodeT *ground, 
	      long ngroundarcs, bucketT *bkts, long nflow, 
	      void **costs, short ***incrcosts, long nrow, long ncol, 
	      paramT *params);
nodeT *FindApex(nodeT *from, nodeT *to);
int CandidateCompare(const void *c1, const void *c2);
nodeT *NeighborNode(nodeT *node1, long arcnum, nodeT **nodes, 
		    nodeT *ground, long *arcrowptr, long *arccolptr, 
		    long *arcdirptr, long nrow, long ncol);
void GetArc(nodeT *from, nodeT *to, long *arcrow, long *arccol, 
	    long *arcdir, long nrow, long ncol);
void NonDegenUpdateChildren(nodeT *startnode, nodeT *lastnode, 
			    nodeT *nextonpath, long dgroup, 
			    long ngroundarcs, long nflow, nodeT **nodes,
			    nodeT *ground, nodeT ***apexes, 
			    void **costs, short ***incrcosts, 
			    long nrow, long ncol, paramT *params);
void InitNetwork(short ***rowflowptr, short ***colflowptr, short **flows, 
		 long *ngroundarcsptr, long *ncycleptr, long *nflowdoneptr, 
		 long *mostflowptr, long *nflowptr, long *candidatebagsizeptr, 
		 candidateT **candidatebagptr, long *candidatelistsizeptr, 
		 candidateT **candidatelistptr, signed char ***iscandidateptr, 
		 nodeT ****apexesptr, bucketT **bktsptr, 
		 long *iincrcostfileptr, short ***incrcosts, 
		 nodeT ***nodesptr, nodeT *ground,
		 long nrow, long ncol, paramT *params);
void InitNodeNums(long nrow, long ncol, nodeT **nodes, nodeT *ground);
void InitBuckets(bucketT *bkts, nodeT *source, long nbuckets);
void InitNodes(long nrow, long ncol, nodeT **nodes, nodeT *ground);
void InitTreeNodes(long nrow, long ncol, nodeT **nodes, nodeT *ground);
void InitArcs(long nrow, long ncol, nodeT ***apexes, 
	      signed char **iscandidate);
void BucketInsert(nodeT *node, long ind, bucketT *bkts);
void BucketRemove(nodeT *node, long ind, bucketT *bkts);
nodeT *ClosestNode(bucketT *bkts);
nodeT *MinOutCostNode(bucketT *bkts);
nodeT *SelectSource(nodeT **nodes, nodeT *ground, long nflow, 
		    short **flows, long ngroundarcs, 
		    long nrow, long ncol, paramT *params);
short GetCost(short ***incrcosts, long arcrow, long arccol, 
	      long arcdir);
long ReCalcCost(void **costs, short ***incrcosts, long flow, 
		long arcrow, long arccol, long nflow, long nrow, 
		paramT *params);
void SetupIncrFlowCosts(void **costs, short ***incrcosts, short **flows,
			long nflow, long nrow, long ncol, 
			paramT *params);
long long EvaluateTotalCost(void **costs, short **flows, 
			    long nrow, long ncol, paramT *params);
void MSTInitFlows(float **wrappedphase, short ***flowsptr, 
		  short **mstcosts, long nrow, long ncol, 
		  nodeT ***nodes, nodeT *ground, long maxflow);
void SolveMST(nodeT **nodes, nodeT *source, nodeT *ground, 
	      bucketT *bkts, short **mstcosts, signed char **residue, 
	      signed char **arcstatus, long nrow, long ncol);
long DischargeTree(nodeT *source, short **mstcosts, short **flows,
		   signed char **residue, signed char **arcstatus, 
		   nodeT **nodes, nodeT *ground, long nrow, long ncol);
signed char ClipFlow(signed char **residue, short **flows, 
		     short **mstcosts, long nrow, long ncol, 
		     long maxflow);
void MCFInitFlows(float **wrappedphase, short ***flowsptr, short **mstcosts, 
		  long nrow, long ncol, long cs2scalefactor);


/* functions in snaphu_cost.c */

void BuildCostArrays(void ***costsptr, short ***mstcostsptr, 
		     float **mag, float **wrappedphase, 
		     float **unwrappedest, char *ampfile, 
		     char *ampfile2, char *weightfile, char *corrfile, 
		     long linelen, long nlines, long nrow, long ncol, 
		     paramT *params);
void GetIntensityAndCorrelation(float **mag, float **wrappedphase, 
				float ***pwrptr, float ***corrptr, 
				char *ampfile, char *ampfile2, 
				char *corrfile, long linelen, long nlines,
				long nrow, long ncol, paramT *params);
void RemoveMean(float **ei, long nrow, long ncol, 
                long krowei, long kcolei);
double SolveDZRCrit(paramT *params, double threshold);
void SolveEIModelParams(double *slope1ptr, double *slope2ptr, 
			double *const1ptr, double *const2ptr, 
			double dzrcrit, paramT *params);
double EIofDZR(double dzr, paramT *params);
double CalcDzRhoMax(double rho, paramT *params, double threshold);
void CalcCostTopo(void **costs, long flow, long arcrow, long arccol, 
		  long nflow, long nrow, paramT *params, 
		  long *poscostptr, long *negcostptr);
void CalcCostDefo(void **costs, long flow, long arcrow, long arccol, 
		  long nflow, long nrow, paramT *params, 
		  long *poscostptr, long *negcostptr);
void CalcCostL0(void **costs, long flow, long arcrow, long arccol, 
		long nflow, long nrow, paramT *params, 
		long *poscostptr, long *negcostptr);
void CalcCostL1(void **costs, long flow, long arcrow, long arccol, 
		long nflow, long nrow, paramT *params, 
		long *poscostptr, long *negcostptr);
void CalcCostL2(void **costs, long flow, long arcrow, long arccol, 
		long nflow, long nrow, paramT *params, 
		long *poscostptr, long *negcostptr);
void CalcCostLP(void **costs, long flow, long arcrow, long arccol, 
		long nflow, long nrow, paramT *params, 
		long *poscostptr, long *negcostptr);
long EvalCostTopo(void **costs, short **flows, long arcrow, long arccol,
		  long nrow, paramT *params);
long EvalCostDefo(void **costs, short **flows, long arcrow, long arccol,
		  long nrow, paramT *params);
long EvalCostL0(void **costs, short **flows, long arcrow, long arccol,
		long nrow, paramT *params);
long EvalCostL1(void **costs, short **flows, long arcrow, long arccol,
		long nrow, paramT *params);
long EvalCostL2(void **costs, short **flows, long arcrow, long arccol,
		long nrow, paramT *params);
long EvalCostLP(void **costs, short **flows, long arcrow, long arccol,
		long nrow, paramT *params);
void CalcInitMaxFlow(paramT *params, void **costs, 
			    long nrow, long ncol);


/* functions in snaphu_util.c */

int IsTrue(char *str);
int IsFalse(char *str);
float ModDiff(float f1, float f2);
void WrapPhase(float **wrappedphase, long nrow, long ncol);
void CycleResidue(float **phase, signed char **residue, 
		  int nrow, int ncol);
void CalcFlow(float **phase, short ***flowsptr, long nrow, long ncol);
void IntegratePhase(float **psi, float **phi, short **flows,
		    long nrow, long ncol);
float **ExtractFlow(float **unwrappedphase, short ***flowsptr, 
		    long nrow, long ncol);
void FlipPhaseArraySign(float **arr, double bperp, 
			long nrow, long ncol);
void FlipFlowArraySign(short **arr, double bperp, long nrow, long ncol);
void **Get2DMem(int nrow, int ncol, int psize, size_t size);
void **Get2DRowColMem(long nrow, long ncol, int psize, size_t size);
void **Get2DRowColZeroMem(long nrow, long ncol, int psize, size_t size);
void *MAlloc(size_t size);
void *CAlloc(size_t nitems, size_t size);
void *ReAlloc(void *ptr, size_t size);
void Free2DArray(void **array, unsigned int nrow);
void FreeParamT(paramT *params);
void Set2DShortArray(short **arr, long nrow, long ncol, long value);
signed char ValidDataArray(float **arr, long nrow, long ncol);
signed char IsFinite(double d);
long FRound(float a);
void Short2DRowColAbsMax(short **arr, long nrow, long ncol, long *initval);
void Despeckle(float **mag, float ***ei, long nrow, long ncol);
float **MirrorPad(float **array1, long nrow, long ncol, long krow, long kcol);
void BoxCarAvg(float **avgarr, float **padarr, long nrow, long ncol, 
	       long krow, long kcol);
char *StrNCopy(char *dest, const char *src, size_t n);
void FlattenWrappedPhase(float **wrappedphase, float **unwrappedest, 
			 long nrow, long ncol);
void Add2DFloatArrays(float **arr1, float **arr2, long nrow, long ncol);
int StringToDouble(char *str, double *d);
int StringToLong(char *str, long *l);
void SetDump(int signum);
void StartTimers(time_t *tstart, double *cputimestart);
void DisplayElapsedTime(time_t tstart, double cputimestart);


/* functions in snaphu_io.c */

void SetDefaults(char *outfile, char *weightfile, char *ampfile, 
		 char *ampfile2, char *magfile, char *corrfile, 
		 char *estfile, paramT *params);
void ProcessArgs(int argc, char *argv[], char *infile, long *ncolptr, 
		 char *outfile, char *weightfile, char *ampfile, 
		 char *ampfile2, char *magfile, char *corrfile, 
		 char *estfile, paramT *params);
void CheckParams(char *infile, char *outfile, char *weightfile, 
		 char *ampfile, char *ampfile2, char *magfile, 
		 char *corrfile, long linelen, long nlines, paramT *params);
void ReadConfigFile(char *conffile, char *infile, char *outfile, 
		    char *weightfile, char *ampfile, char *ampfile2, 
		    char *magfile, char *corrfile, char *estfile, 
		    long *ncolptr, paramT *params);
void SetTileParams(char *infile, long linelen, long *nlinesptr, 
		   long *nrowptr, long *ncolptr, paramT *params);
void WriteOutputFile(float **mag, float **unwrappedphase, char *outfile,
		     long nrow, long ncol, paramT *params);
FILE *OpenOutputFile(char *outfile);
void WriteAltLineFile(float **mag, float **phase, char *outfile, 
		      long nrow, long ncol);
void WriteAltSampFile(float **arr1, float **arr2, char *outfile, 
		      long nrow, long ncol);
void Write2DArray(void **array, char *filename, long nrow, long ncol, 
		  size_t size);
void Write2DRowColArray(void **array, char *filename, long nrow, 
			long ncol, size_t size);
void ReadInputFile(char *infile, float ***magptr, float ***wrappedphaseptr, 
		   short ***flowsptr, long linelen, long nlines, 
		   paramT *params);
void ReadMagnitude(float **mag, char *magfile, long linelen, long nlines, 
		   paramT *params);
void ReadUnwrappedEstimateFile(float ***unwrappedestptr, char *estfile, 
			       long linelen, long nlines, paramT *params);
void ReadWeightsFile(short ***weightsptr,char *weightfile, 
			    long linelen, long nlines, paramT *params);
void ReadIntensity(float ***pwrptr, float ***pwr1ptr, float ***pwr2ptr, 
		   char *ampfile, char *ampfile2, long linelen, long nlines, 
		   paramT *params);
void ReadCorrelation(float ***corrptr, char *corrfile, 
		     long linelen, long nlines, paramT *params);
void ReadAltLineFile(float ***mag, float ***phase, char *mpfile, 
		     long linelen, long nlines, paramT *params);
void ReadAltLineFilePhase(float ***phase, char *mpfile, 
			  long linelen, long nlines, paramT *params);
void ReadComplexFile(float ***mag, float ***phase, char *rifile, 
		     long linelen, long nlines, paramT *params);
void ReadFloatFile(float ***arr, char *infile, 
		   long linelen, long nlines, paramT *params);
void ReadAltSampFile(float ***arr1, float ***arr2, char *infile,
		     long linelen, long nlines, paramT *params);
void Read2DRowColFile(void ***arr, char *filename, long linelen, long nlines, 
		      paramT *params, size_t size);
void SetDumpAll(paramT *params);
void SetStreamPointers(void);
void SetVerboseOut(paramT *params);
void DumpIncrCostFiles(short ***incrcosts, long iincrcostfile, 
		       long nflow, long nrow, long ncol);


/* functions in snaphu_cs2.c  */

void SolveCS2(signed char **residue, short **mstcosts, long nrow, long ncol, 
	      long cs2scalefactor, short ***flowsptr);



/*******************************************/
/* global (external) variable declarations */
/*******************************************/

/* flags used for signal handling */
extern char dumpresults_global;
extern char requestedstop_global;

/* ouput stream pointers */
/* sp0 is for error messages, sp1 for status output, and sp2 for verbose */
extern FILE *sp0, *sp1, *sp2;

/* node pointer for marking arc not on tree in apex array */
/* this should be treat as a constant */
extern nodeT NONTREEARC[1];

/* pointers to functions which calculate arc costs */
extern void (*CalcCost)();
extern long (*EvalCost)();



/* end of snaphu.h */




