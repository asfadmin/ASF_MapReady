#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <signal.h>
#include <limits.h>
#include <float.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

#include "snaphu.h"


/* to do */
/*
 
 * update man page, add troubleshooting section

 * twiddle rhominfactor (1.4) for best speed with auto corr generation 

 * boundary conditions; change file i/o to do patches?
 
 * figure out how to get rid of fringes in water of Falk's data

 * speed: when updating node potentials and tree structure in TreeSolve(), 
 * keep list of arcs that need apexes updated (and need reduced costs 
 * rechecked).  Then below, we can just read from the list.  We can also 
 * sort the list and do arcs from nodes at the lowest tree level first, then
 * create new groups and add entries to apexlist, making apex searches easier.

 * automatic selection of init alg (MST, MCF) based on correlation 

 * need to normalize weights before multiply costs by them?

 * make sure weights scaled okay when correlation high (small sigsq)
 * need autoscaling for costs, based on weights, params
   just make sure cost->sigsq below thresh (scale so max is at max allowed)
   and cost->laycost also below max

 * log file

 * -r to read roi.in file
 * -E ERS params
 * -R Radarsat params
 * -J JERS params

 * Error check: make sure groupcounter doesn't overflow!
 * make sure incost, outcost don't overflow long 
 * change incost and outcost to type long long ???
 */



/* global (external) variable definitions */

/* flags used for signal handling */
char dumpresults_global;
char requestedstop_global;

/* ouput stream pointers */
/* sp0 is for error messages, sp1 for status output, and sp2 for verbose */
FILE *sp0, *sp1, *sp2;

/* node pointer for marking arc not on tree in apex array */
/* this should be treated as a constant */
nodeT NONTREEARC[1];

/* pointers to functions which calculate arc costs */
void (*CalcCost)();
long (*EvalCost)();



/***************************/
/* main program for snaphu */
/***************************/

int main(int argc, char **argv){


  /* variable declarations */
  long nrow, ncol, linelen, nlines;
  long n, ngroundarcs, iincrcostfile;
  long nflow, ncycle, mostflow, nflowdone;
  long candidatelistsize, candidatebagsize;
  short **flows, **mstcosts;
  short **rowflow, **colflow;
  short **incrcosts[2];
  float **wrappedphase, **unwrappedphase, **mag, **unwrappedest;
  void **costs;
  long long totalcost, oldtotalcost;
  nodeT *source, ***apexes;
  nodeT **nodes, ground[1];
  candidateT *candidatebag, *candidatelist;
  char infile[MAXSTRLEN], outfile[MAXSTRLEN], magfile[MAXSTRLEN];
  char ampfile[MAXSTRLEN], ampfile2[MAXSTRLEN];
  char weightfile[MAXSTRLEN], corrfile[MAXSTRLEN], estfile[MAXSTRLEN];
  signed char **iscandidate;
  signed char notfirstloop;
  paramT *params;
  bucketT *bkts;
  time_t tstart;
  double cputimestart;


  /* get current wall clock and CPU time */
  StartTimers(&tstart,&cputimestart);

  /* set output stream pointers (may be reset after inputs parsed) */
  SetStreamPointers();

  /* print greeting */
  fprintf(sp1,"\n%s v%s\n",PROGRAMNAME,VERSION);

  /* get memory for parameter data structure */
  params=MAlloc(sizeof(paramT));

  /* set default parameters */
  SetDefaults(outfile,weightfile,ampfile,ampfile2,magfile,corrfile,
	      estfile,params);
  ReadConfigFile(DEF_SYSCONFFILE,infile,outfile,weightfile,ampfile,ampfile2,
		 magfile,corrfile,estfile,&linelen,params);

  /* parse the command line inputs */
  ProcessArgs(argc,argv,infile,&linelen,outfile,weightfile,ampfile,ampfile2,
	      magfile,corrfile,estfile,params);

  /* set verbose output if specified */
  SetVerboseOut(params);

  /* take care of tile parameters */
  SetTileParams(infile,linelen,&nlines,&nrow,&ncol,params);

  /* check validity of parameters */
  CheckParams(infile,outfile,weightfile,ampfile,ampfile2,
	      magfile,corrfile,linelen,nlines,params);

  /* set names of dump files if necessary */
  SetDumpAll(params);

  /* read input file (memory allocated by read function) */
  ReadInputFile(infile,&mag,&wrappedphase,&flows,linelen,nlines,params);

  /* read interferogram magnitude if specified separately */
  ReadMagnitude(mag,magfile,linelen,nlines,params);

  /* read the coarse unwrapped estimate, if provided */
  unwrappedest=NULL;
  if(strlen(estfile)){
    ReadUnwrappedEstimateFile(&unwrappedest,estfile,linelen,nlines,params);

    /* subtract the estimate from the wrapped phase (and re-wrap) */
    FlattenWrappedPhase(wrappedphase,unwrappedest,nrow,ncol);

  }

  /* build the cost arrays */  
  BuildCostArrays(&costs,&mstcosts,mag,wrappedphase,unwrappedest,
		  ampfile,ampfile2,weightfile,corrfile,
		  linelen,nlines,nrow,ncol,params);


  /* if in quantify-only mode, evaluate cost of unwrapped input then exit */
  if(params->eval){
    Short2DRowColAbsMax(flows,nrow,ncol,&mostflow);
    fprintf(sp1,"Maximum flow on network: %ld\n",mostflow);
    totalcost=EvaluateTotalCost(costs,flows,nrow,ncol,params);
    fprintf(sp1,"Total solution cost: %.9g\n",(double )totalcost);
    Free2DArray((void **)costs,2*nrow-1);
    Free2DArray((void **)mag,nrow);
    Free2DArray((void **)wrappedphase,nrow);
    Free2DArray((void **)flows,2*nrow-1);
    FreeParamT(params);
    exit(NORMAL_EXIT);
  }

  /* initialize the flows (find simple unwrapping to get a feasible flow) */
  unwrappedphase=NULL;
  nodes=NULL;
  if(!params->unwrapped){

    /* see which initialization method to use */
    if(params->initmethod==MSTINIT){

      /* use minimum spanning tree (MST) algorithm */
      MSTInitFlows(wrappedphase,&flows,mstcosts,nrow,ncol,
		   &nodes,ground,params->initmaxflow);
    
    }else if(params->initmethod==MCFINIT){

      /* use minimum cost flow (MCF) algorithm */
      MCFInitFlows(wrappedphase,&flows,mstcosts,nrow,ncol,
		   params->cs2scalefactor);

    }else{
      fprintf(sp0,"Illegal initialization method.  Abort.\n");
      exit(ABNORMAL_EXIT);
    }

    /* integrate the phase and write out if necessary */
    if(params->initonly || params->initfile!=NULL){
      fprintf(sp1,"Integrating phase\n");
      unwrappedphase=(float **)Get2DMem(nrow,ncol,
					sizeof(float *),sizeof(float));
      IntegratePhase(wrappedphase,unwrappedphase,flows,nrow,ncol);
      if(unwrappedest!=NULL){
	Add2DFloatArrays(unwrappedphase,unwrappedest,nrow,ncol);
      }
      FlipPhaseArraySign(unwrappedphase,params->bperp,nrow,ncol);

      /* exit if called in init only; otherwise, free memory and continue */
      if(params->initonly){
	WriteOutputFile(mag,unwrappedphase,outfile,nrow,ncol,params);  
	fprintf(sp1,"Output written to file %s\n",outfile);
	fprintf(sp1,"Program %s done.\n",PROGRAMNAME);
	DisplayElapsedTime(tstart,cputimestart);
	exit(NORMAL_EXIT);
      }else{
	WriteOutputFile(mag,unwrappedphase,params->initfile,nrow,ncol,
			params);  
	Free2DArray((void **)unwrappedphase,nrow);
      }
    }
  }

  /* initialize network variables */
  InitNetwork(&rowflow,&colflow,flows,&ngroundarcs,&ncycle,&nflowdone,
	      &mostflow,&nflow,&candidatebagsize,&candidatebag,
	      &candidatelistsize,&candidatelist,&iscandidate,
	      &apexes,&bkts,&iincrcostfile,incrcosts,&nodes,ground,
	      nrow,ncol,params);

  /* trap interrupt and hangup signals (call SetDump() if signals received) */
  /* does this affect speed? */
  dumpresults_global=FALSE;
  requestedstop_global=FALSE;
  signal(SIGINT,SetDump);
  signal(SIGHUP,SetDump);

  /* main loop: loop over flow increments and sources */
  fprintf(sp1,"Running nonlinear network flow optimizer\n");
  fprintf(sp1,"Maximum flow on network: %ld\n",mostflow);
  notfirstloop=FALSE;
  totalcost=LARGELONGLONG;
  while(TRUE){ 
 
    fprintf(sp1,"Flow increment: %ld  (Total improvements: %ld)\n",
	    nflow,ncycle);

    /* set up the incremental (residual) cost arrays */
    SetupIncrFlowCosts(costs,incrcosts,flows,nflow,nrow,ncol,params); 
    if(params->dumpall){
      DumpIncrCostFiles(incrcosts,++iincrcostfile,nflow,nrow,ncol);
    }

    /* set the tree root (equivalent to source of shortest path problem) */
    source=SelectSource(nodes,ground,nflow,flows,ngroundarcs,
			nrow,ncol,params);
    
    /* run the solver, and increment nflowdone if no cycles are found */
    n=TreeSolve(nodes,ground,source,&candidatelist,&candidatebag,
		&candidatelistsize,&candidatebagsize,
		bkts,flows,costs,incrcosts,apexes,iscandidate,
		ngroundarcs,nflow,mag,wrappedphase,outfile,nrow,ncol,params);
    
    /* evaluate and save the total cost (skip if first loop through nflow) */
    if(notfirstloop){
      oldtotalcost=totalcost;
      totalcost=EvaluateTotalCost(costs,flows,nrow,ncol,params);
      if(totalcost>oldtotalcost || (n>0 && totalcost==oldtotalcost)){
	fprintf(sp0,"Unexpected increase in total cost.  Breaking loop.\n");
	break;
      }
    }

    /* consider this flow increment done if no neg cycles found */
    ncycle+=n;
    if(n<=params->maxnflowcycles){
      nflowdone++;
    }else{
      nflowdone=1;
    }

    /* find maximum flow on network */
    mostflow=0;
    Short2DRowColAbsMax(flows,nrow,ncol,&mostflow);

    /* break if we're done with all flow increments or problem is convex */
    if(nflowdone>=params->maxflow || nflowdone>=mostflow || params->p>=1.0){
      break;
    }

    /* update flow increment */
    nflow++;
    if(nflow>params->maxflow || nflow>mostflow){
      nflow=1;
      notfirstloop=TRUE;
    }
    fprintf(sp2,"Maximum flow on network: %ld\n",mostflow);

    /* dump flow arrays if necessary */
    if(params->flowfile!=NULL){
      FlipFlowArraySign(flows,params->bperp,nrow,ncol);
      Write2DRowColArray((void **)flows,params->flowfile,nrow,ncol,
			 sizeof(short));
      FlipFlowArraySign(flows,params->bperp,nrow,ncol);
    }

  } /* end loop until no more neg cycles */

  /* free some memory */
  Free2DArray((void **)nodes,nrow-1);
  Free2DArray((void **)apexes,2*nrow-1);
  Free2DArray((void **)iscandidate,2*nrow-1);
  Free2DArray((void **)incrcosts[POSINCR],2*nrow-1);
  Free2DArray((void **)incrcosts[NEGINCR],2*nrow-1);
  free(candidatebag);
  free(candidatelist);
  free(bkts->bucketbase);

  /* evaluate and display the maximum flow and total cost */
  totalcost=EvaluateTotalCost(costs,flows,nrow,ncol,params);
  fprintf(sp1,"Maximum flow on network: %ld\n",mostflow);
  fprintf(sp1,"Total solution cost: %.9g\n",(double )totalcost);

  /* integrate the wrapped phase with the solution flow */
  fprintf(sp1,"Integrating phase\n");
  unwrappedphase=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
  IntegratePhase(wrappedphase,unwrappedphase,flows,nrow,ncol);

  /* reinsert the coarse estimate, if it was given */
  if(unwrappedest!=NULL){
    Add2DFloatArrays(unwrappedphase,unwrappedest,nrow,ncol);
  }

  /* flip the sign of the unwrapped phase array if it was flipped initially, */
  /* then write the unwrapped output */
  FlipPhaseArraySign(unwrappedphase,params->bperp,nrow,ncol);
  WriteOutputFile(mag,unwrappedphase,outfile,nrow,ncol,params);  
  fprintf(sp1,"Output written to file %s\n",outfile);

  /* free remaining memory and exit */
  Free2DArray((void **)costs,2*nrow-1);
  Free2DArray((void **)mag,nrow);
  Free2DArray((void **)wrappedphase,nrow);
  Free2DArray((void **)unwrappedphase,nrow);
  Free2DArray((void **)flows,2*nrow-1);
  FreeParamT(params);
  fprintf(sp1,"Program %s done.\n",PROGRAMNAME);
  DisplayElapsedTime(tstart,cputimestart);
  exit(NORMAL_EXIT);


} /* end of main() */



