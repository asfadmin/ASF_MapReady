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
#include "tilesnaphu.h"



/* to do:

 * log file --log option.  cat log files together at end

 * save option --save to save intermediate files.

 * man page

 * trap signals and send them to child processes (individual snaphu calls)
   kill children and remove intermediate files, exit if interrupt

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




/*******************************/
/* main program for tilesnaphu */
/*******************************/
/*
 * Add arguments for a call to demIFM.
 * -demifm demargfile 
 *   demargfile contents:
 *   <igram_byte image.ext> <meta file> <base file> <dem file.ext>
 *   igram_byte.img metaFile base.00 dem.img
 *
 *  if this is not done the new dem phase file will not be generated.
 *
 */

int main(int argc, char **argv){

  FILE *fp;
  paramT *params;
  long i, snaphuargc, row, col;
  long filesize, datasize, linelen, nlines, nrow, ncol, rowovrlp, colovrlp;
  long ni, nj, nilastrow, njlastcol, istart, jstart, numi, numj, sleepinterval;
  float **mag, **unwphase;
  char **snaphuargv, **snaphucallarr;
  char ***unwfilearr;
  char infile[MAXSTRLEN], outfile[MAXSTRLEN], tmpstring[MAXSTRLEN];
  char snaphuoptions[MAXSTRLEN], snaphupath[MAXSTRLEN], outfileroot[MAXSTRLEN];
  char noarg_exit, quiet, printwarning;
  char sysCall[MAXSTRLEN];
  long npthreads;
  time_t tstart;
  double cputimestart;

/* Dave Koster added these to use demIFM */

  char igramByteFile[MAXSTRLEN], metaFile[MAXSTRLEN];
  char baselineFile[MAXSTRLEN], demFile[MAXSTRLEN];
  int dodemIFM;

/* End added stuff */

#ifdef USE_PTHREADS
  pthread_mutex_t nextcallmutex[1];
  pthread_t *pthreadarr;
  long pthreadnum;
  void *pthreadstatus[1];
  callsnaphuargT callsnaphuarg[1];
#endif

  /* get current wall clock and CPU time */
  StartTimers(&tstart,&cputimestart);

  /* set output stream pointers (may be reset after inputs parsed) */
  SetStreamPointers();

  /* get memory for parameter data structure */
  params=MAlloc(sizeof(paramT));

  /* set default parameters (this is just to get the input file format) */
  quiet=FALSE;
  npthreads=DEF_NPTHREADS;
  SetDefaults(outfile,tmpstring,tmpstring,tmpstring,tmpstring,tmpstring,
              tmpstring,params);
  ReadConfigFile(DEF_SYSCONFFILE,infile,outfile,tmpstring,tmpstring,tmpstring,
		 tmpstring,tmpstring,tmpstring,&linelen,params);

  /* process tile parameters from input command line */
  if(argc<5){
    fprintf(sp0,TILEUSEMESSAGE);
    exit(ABNORMAL_EXIT);
  }
  i=0;
  if(StringToLong(argv[++i],&ncol) 
     || StringToLong(argv[++i],&nrow)
     || StringToLong(argv[++i],&colovrlp) 
     || StringToLong(argv[++i],&rowovrlp)){
    fprintf(sp0,"tile parameters must be integer\n");
    exit(ABNORMAL_EXIT);
  }

  /* look for any other tiling options */
  strcpy(snaphupath,"");
  while(++i<argc){
    noarg_exit=FALSE;
    if(!strcmp(argv[i],"--path")){
      if(++i<argc){
	StrNCopy(snaphupath,argv[i],MAXSTRLEN);
      }else{
	noarg_exit=TRUE;
      }
    }else if(!strcmp(argv[i],"--quiet")){
      quiet=TRUE;
    }else if(!strcmp(argv[i],"--nproc")){
      if(++i<argc){
	if(StringToLong(argv[i],&npthreads)){
	  fprintf(sp0,"number of processors must be an integer\n");
	  exit(ABNORMAL_EXIT);
	}
#ifndef USE_PTHREADS
      fprintf(sp0,"Program not compiled for multiple threads.  "
	      "--nproc option ignored\n");
#endif
      }else{
	noarg_exit=TRUE;
      }
/* Added by Dave Koster ------------------------------------------------*/
    }else if(!strcmp(argv[i], "--demifm")){
	FILE *fPtr;
	char tmpin[MAXSTRLEN];
	if(++i<argc){
	  strcpy(tmpin, argv[i]);
	  fPtr = fopen(tmpin, "r");
	  fscanf(fPtr, "%s %s %s %s", igramByteFile, metaFile, 
				      baselineFile, demFile);
		dodemIFM = 1;
	  fclose(fPtr);
	}else{
	  noarg_exit=TRUE;
	}
/* End Added by Dave Koster -------------------------------------------*/
    }else{
      break;
    }
    if(noarg_exit){
      fprintf(sp0,"incorrect number of arguments for option %s\n",
	      argv[i-1]);
      exit(ABNORMAL_EXIT);
    }
  }

  /* copy snaphu options to a string */
  i--;
  snaphuargc=argc-i;
  snaphuargv=&(argv[i]);
  strcpy(snaphuoptions,"");
  for(i=1;i<snaphuargc;i++){
    strcat(snaphuoptions,snaphuargv[i]);
    strcat(snaphuoptions," ");
  }

  /* pass snaphu arguments to snaphu parser */
  ProcessArgs(snaphuargc,snaphuargv,infile,&linelen,outfile,tmpstring,
	      tmpstring,tmpstring,tmpstring,tmpstring,tmpstring,params);

  /* get number of lines of input file */
  if((fp=fopen(infile,"r"))==NULL){
    fprintf(sp0,"unable to read from file %s\n",infile);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_END);
  filesize=ftell(fp);
  fclose(fp);
  if(params->infileformat==FLOAT_DATA){
    datasize=sizeof(float);
  }else if(params->infileformat==COMPLEX_DATA 
	   || params->infileformat==ALT_LINE_DATA
	   || params->infileformat==ALT_SAMPLE_DATA){
    datasize=2*sizeof(float);
  }else{
    fprintf(sp0,"illegal input file format\n");
    exit(ABNORMAL_EXIT);
  }
  if(filesize % (linelen*datasize)){
    fprintf(sp0,"extra data at end of file %s\n",infile);
    exit(ABNORMAL_EXIT);
  }
  nlines=filesize/(linelen*datasize);

  /* disallow other output files (dump files, etc.) */
  printwarning=FALSE;
  if(params->dumpall){
    printwarning=TRUE;
  }
  UnsetDumpFile(&(params->initfile),&printwarning);
  UnsetDumpFile(&(params->flowfile),&printwarning);
  UnsetDumpFile(&(params->eifile),&printwarning);
  UnsetDumpFile(&(params->rowcostfile),&printwarning);
  UnsetDumpFile(&(params->colcostfile),&printwarning);
  UnsetDumpFile(&(params->mstrowcostfile),&printwarning);
  UnsetDumpFile(&(params->mstcolcostfile),&printwarning);
  UnsetDumpFile(&(params->corrdumpfile),&printwarning);
  if(printwarning){
    fprintf(sp1,"WARNING: intermediate snaphu arrays will be overwritten\n");
    fprintf(sp1,"         when used with tilesnaphu\n");
  }

  /* calculate tile parameters */
  fprintf(sp1,"Calculating tile parameters\n");
  ni=ceil((nlines+(nrow-1)*rowovrlp)/(double )nrow);
  nj=ceil((linelen+(ncol-1)*colovrlp)/(double )ncol);
  nilastrow=nlines-(nrow-1)*(ni-rowovrlp);
  njlastcol=linelen-(ncol-1)*(nj-colovrlp);
  sleepinterval=FRound(ni*nj*SECONDSPERPIXEL);
  
  /* get the path to snaphu executable and append executable name */
  if(strlen(snaphupath)){
    i=strlen(snaphupath);
    if(snaphupath[i-1]!='/'){
      snaphupath[i-1]='\0';
    }
    strncat(snaphupath,"snaphu",MAXSTRLEN);
  }else{
    StrNCopy(snaphupath,"snaphu",MAXSTRLEN);
  }

  /* create array of snaphu call strings */
  sprintf(outfileroot,"tmptileout_%s",outfile);
  snaphucallarr=MAlloc(nrow*ncol*sizeof(char *));
  unwfilearr=MAlloc(nrow*sizeof(char **));
  i=0;
  for(row=0;row<nrow;row++){
    unwfilearr[row]=MAlloc(ncol*sizeof(char *));
    for(col=0;col<ncol;col++){
      snaphucallarr[i]=MAlloc(2*MAXSTRLEN*sizeof(char));
      unwfilearr[row][col]=MAlloc(MAXSTRLEN*sizeof(char));

      /* set starting indices of tile */
      istart=row*(ni-rowovrlp);
      jstart=col*(nj-colovrlp);

      /* set number of rows and columns in tile */
      if(row==nrow-1){
	numi=nilastrow;
      }else{
	numi=ni;
      }
      if(col==ncol-1){
	numj=njlastcol;
      }else{
	numj=nj;
      }

      /* create the snaphu call */
      /* -o at end of command supersedes any previous output specifications */
      sprintf(tmpstring,"%s_%ld_%ld.%ld",outfileroot,row,col,numj);
      StrNCopy(unwfilearr[row][col],tmpstring,MAXSTRLEN);
      sprintf(snaphucallarr[i],"%s %s --tile %ld %ld %ld %ld -o %s",
	      snaphupath,snaphuoptions,jstart+1,istart+1,numj,numi,
	      unwfilearr[row][col]);
      if(quiet){
	strcat(snaphucallarr[i]," > /dev/null");
      }
      i++;
    }
  }


#ifdef DEBUG
  printf("\nsnaphu calls:\n");
  for(i=0;i<nrow*ncol;i++){
    printf("%s\n",snaphucallarr[i]);
  }
  if((fp=fopen("snaphucalls.txt","w"))!=NULL){
    fprintf(fp,"snaphu calls:\n");
    for(i=0;i<nrow*ncol;i++){
      fprintf(fp,"%s\n",snaphucallarr[i]);
    }
    fclose(fp);
  }

#endif

/*------  Added by Dave Koster ----------------------------------------*/
 /* Vars for tile: */
 /*     numi(num cols), istart(starting col)*/
 /*     numj(num rows), jstart(starting row)
  *	igramByteFile[MAXSTRLEN], metaFile[MAXSTRLEN];
  *	baselineFile[MAXSTRLEN], demFile[MAXSTRLEN];
  *
 */
 /* Create a file using trim from the data we are tiling out. */
if(dodemIFM == 1)
{

	/* Vars for this dodemIFM block of code only!! */

	 /* char igramByteFile[MAXSTRLEN], metaFile[MAXSTRLEN]; */
  	/* char baselineFile[MAXSTRLEN], demFile[MAXSTRLEN]; */

	char tempchar[MAXSTRLEN];
	char arglist[255];
	char newarglist[255];
	
	int ptr, ii, nn, arglen;

	/* if out_dem_seeds exists, remove it! */
	system("rm out_dem_seeds");

	i = 0;
	/* Use this to modify snaphu calls, ect.... */
 	 printf("\nsnaphu calls:\n");
	for(row=0;row<nrow;row++){
 	  for(col=0;col<ncol;col++){

	  /* set starting indices of tile */
      	    istart=row*(ni-rowovrlp);
            jstart=col*(nj-colovrlp);

          /* set number of rows and columns in tile */
            if(row==nrow-1){
    	      numi=nilastrow;
            }else{
	      numi=ni;
            }
            if(col==ncol-1){
	      numj=njlastcol;
            }else{
	     numj=nj;
            }
	
	    sprintf(sysCall, "trim %s tile_%ld_%ld_%ld %ld %ld %ld %ld", 
				igramByteFile, row, col, numj, istart+1, jstart+1,
				numi, numj);

	    printf("%s\n", sysCall);

	    if(system(sysCall)){
	       	 fprintf(sp0,"Error while trying to call trim(1).\n");
    	         exit(ABNORMAL_EXIT);
	    }

	    sprintf(sysCall, "ak_demIFM tile_%ld_%ld_%ld.img %s %s %s ",
			row, col, numj, metaFile, baselineFile, demFile);
	    sprintf(tempchar, "out_dem_phase_%ld_%ld_%ld.phase", row, col, numj);
	    strcat(sysCall, tempchar);
	    sprintf(tempchar, " out_dem_seeds_%ld_%ld_%ld", row, col, numj);
	    strcat(sysCall, tempchar);
	    
	    printf("%s\n", sysCall);
	    if(system(sysCall)){
  	      fprintf(sp0,"Error while trying to call ak_demIFM(1).\n");
    	      exit(ABNORMAL_EXIT);
	    }

	    /* Generate the out_dem_seeds file for the whole image, by 
	       concatenating the intermediate tiled dem_seeds files */
	    memset(sysCall, '\0', 255);

	    sprintf(sysCall, "tilesnaphu_catDEMSeeds out_dem_seeds_%ld_%ld_%ld %ld", row, col, numj, (istart + 1) * 5);
	    printf("%s\n", sysCall);
	    if(system(sysCall)){
		fprintf(sp0, "Error while trying to generate the seed point file.\n");
		exit(ABNORMAL_EXIT);
	    }		

	    /* Okay, From here modify the call array so that it reflects the different
		.phase file to be sent to snaphu.  */

	    memset(arglist, '\0', 255);
	    memset(newarglist, '\0', 255);

	    strcpy(arglist, snaphucallarr[i]);

	    /* first, locate the -E, so we can replace the *.phase following it. */ 
	    ptr = 0;
            while((ptr <= strlen(arglist)-2) && (strlen(arglist) > 0)){ 
	      char tmpchar[255];
	      memset(tmpchar, '\0', 255);

              for(ii = ptr; ii < ptr+2; ii++){
                 tmpchar[ii-ptr] = arglist[ii];
              }
	      if(strncmp(tmpchar, "-e", 2) == 0){
		/* make it a -E */\
		arglist[ptr+1] = 'E';
		strncpy(tmpchar, "-E", 2);
	      }
              if(strncmp(tmpchar, "-E", 2) == 0){
                strncpy(newarglist, arglist, ptr + 2);
                ptr += 3;
                sprintf(tmpchar, " out_dem_phase_%ld_%ld_%ld.phase ", row, col, numj);
                strcat(newarglist, tmpchar);
                ii = ptr;

                while(arglist[ii] != ' '){
                  ii++;
		}
                while(ii <= strlen(arglist)){
                  ii++;
                  arglen = strlen(newarglist);
                  newarglist[arglen] = arglist[ii];
                  newarglist[arglen+1] = '\0';
                }
		printf("%s\n", newarglist);
                ptr = strlen(arglist)+1;
              }
	    ptr++;
            }
	    strcpy(snaphucallarr[i], newarglist);
	    i++; 
	  }
        }

 /*  printf("\nsnaphu calls:\n");
  for(i=0;i<nrow*ncol;i++){
    printf("%s\n",snaphucallarr[i]);
  }*/
 /*
  *  Now, just call demIFM to get the correct phase for unwrapping.
  * I require a .meta file here, but I'm not sure where from.
  */

 /* Remove the .phase file in callsnaphuarg, and replace it  */
 /*  with the one just generated with demIFM.		     */
    


 /* Call demIFM with the tile of data.                        */
/*------  End Added by Dave Koster ----------------------------------------*/
}

  /* get ready to call snaphu */
  if(!system(NULL)){
    fprintf(sp0,"system call not available (couldn't find sh)\n");
    exit(ABNORMAL_EXIT);
  }
  fprintf(sp1,"Calling snaphu on tiles\n");

#ifdef USE_PTHREADS

  /* parallel code */
  if(pthread_mutex_init(nextcallmutex,NULL)){
    fprintf(sp0,"pthread_mutex_init() error\n");
    exit(ABNORMAL_EXIT);
  }
  pthreadarr=MAlloc(npthreads*sizeof(pthread_t));
  i=0;

  /* set the inputs to the function each pthread will call */
  callsnaphuarg->iptr=&i;
  callsnaphuarg->snaphucallarr=snaphucallarr;
  callsnaphuarg->nextcallmutex=nextcallmutex;
  callsnaphuarg->ncalls=nrow*ncol;

  /* create threads and run snaphu */
  for(pthreadnum=0;pthreadnum<npthreads;pthreadnum++){
    if(pthread_create(&pthreadarr[pthreadnum],NULL,
		      CallSnaphu,callsnaphuarg)){
      fprintf(sp0,"pthread_create() error\n");
      exit(ABNORMAL_EXIT);
    }

    /* wait a little while for file i/o before starting next thread */
    sleep(sleepinterval);
  }

  /* join threads (wait for all snaphu calls to finish) */
  for(pthreadnum=0;pthreadnum<npthreads;pthreadnum++){
    if(pthread_join(pthreadarr[pthreadnum],pthreadstatus)){
      fprintf(sp0,"pthread_join() error\n");
      exit(ABNORMAL_EXIT);
    }
  }
  if(pthread_mutex_destroy(nextcallmutex)){
    fprintf(sp0,"pthread_mutex_init() error\n");
    exit(ABNORMAL_EXIT);
  }

#else

  /* nonparallel code */
  for(i=0;i<nrow*ncol;i++){
    fprintf(sp1,"\n%s\n",snaphucallarr[i]);
    if(system(snaphucallarr[i])){
      fprintf(sp0,"snaphu call failed.  abort\n");
      exit(ABNORMAL_EXIT);
    }
  }

#endif

  /* reassemble tiles */
  fprintf(sp1,"Reassembling tiles\n");
  mag=NULL;
  unwphase=NULL;
  AssembleTiles(unwfilearr,&mag,&unwphase,linelen,nlines,nrow,ncol,
		rowovrlp,colovrlp,ni,nj,params->outfileformat);

  /* adjust tile phase offsets so that they line up nicely */
  AdjustTileOffsets(unwphase,linelen,nlines,nrow,ncol,
		    rowovrlp,colovrlp,ni,nj);

  /* write out the data */
  WriteOutputFile(mag,unwphase,outfile,nlines,linelen,params);

  /* finish up */
  Free2DArray((void **)unwphase,nlines);
  if(mag!=NULL){
    Free2DArray((void **)mag,nlines);
  }
  Free2DArray((void **)snaphucallarr,nrow);
  FreeParamT(params);
  fprintf(sp1,"Output written to file %s\n",outfile);
  fprintf(sp1,"Program tilesnaphu done.\n");
  DisplayElapsedTime(tstart,cputimestart);
  exit(NORMAL_EXIT);

}


/* function: AssembleTiles()
 * -------------------------
 */
void AssembleTiles(char ***outfilearr, float ***magptr, float ***unwphaseptr, 
		   long linelen, long nlines, long nrow, long ncol, 
		   long rowovrlp, long colovrlp, long ni, long nj, 
		   signed char fileformat){

  FILE *fp;
  long row, col, i, j, ioff, joff, maxni, maxnj, ilimlo, jlimlo, jlimhi;
  long datalinelen;
  float **unwphase, **mag, *inpline;
  char syscall[MAXSTRLEN];

  /* get memory */
  if(*unwphaseptr==NULL){
    unwphase=(float **)Get2DMem(nlines,linelen,sizeof(float *),sizeof(float));
  }else{
    unwphase=*unwphaseptr;
  }
  if(fileformat==ALT_LINE_DATA){
    if(*magptr==NULL){
      mag=(float **)Get2DMem(nlines,linelen,sizeof(float *),sizeof(float));
    }else{
      mag=*magptr;
    }
  }else if(fileformat==FLOAT_DATA){
    mag=NULL;
  }else{
    fprintf(sp0,"incompatible file format passed to AssembleTiles()\n");
    exit(ABNORMAL_EXIT);
  }
  inpline=MAlloc(2*nj*sizeof(float));

  /* loop over all tiles and read in the data */
  maxni=(long )(ni-floor(rowovrlp/2.0));
  for(row=0;row<nrow;row++){
    if(row==0){
      ilimlo=0;
    }else{
      ilimlo=(long )ceil(rowovrlp/2.0);
    }
    if(row==nrow-1){
      maxni=nlines-(nrow-1)*(ni-rowovrlp);
    }
    for(col=0;col<ncol;col++){
      
      /* open the input file */
      if((fp=fopen(outfilearr[row][col],"r"))==NULL){
        fprintf(sp0,"unable to read from file %s.  abort\n",
                outfilearr[row][col]);
        exit(ABNORMAL_EXIT);
      }
      
      /* set column index limits */
      if(col!=ncol-1){
        maxnj=nj;
        jlimhi=(long )(nj-floor(colovrlp/2.0));
      }else{
        maxnj=linelen-(ncol-1)*(nj-colovrlp);
        jlimhi=maxnj;
      }
      if(col==0){
        jlimlo=0;
      }else{
        jlimlo=(long )ceil(colovrlp/2.0);
      }
      ioff=row*(ni-rowovrlp);
      joff=col*(nj-colovrlp);

      /* loop over input lines */
      if(fileformat==ALT_LINE_DATA){
	datalinelen=2*maxnj;
      }else{
	datalinelen=maxnj;
      }
      for(i=0;i<maxni;i++){

        /* read a line of data */
        if(fread(inpline,sizeof(float),datalinelen,fp)!=datalinelen){
          fprintf(sp0,"error reading file %s.  abort\n",
                  outfilearr[row][col]);
          exit(ABNORMAL_EXIT);
        }
        
        /* stuff data into the right place */
        if(i>=ilimlo){
          for(j=jlimlo;j<jlimhi;j++){
	    if(fileformat==ALT_LINE_DATA){
              mag[ioff+i][joff+j]=inpline[j];
	      unwphase[ioff+i][joff+j]=inpline[maxnj+j];
	    }else{
	      unwphase[ioff+i][joff+j]=inpline[j];
	    }
          }
        }
      }

      /* close and remove the input file */
/* ------------- Commented by Dave Koster for Debug Purposes */
//      sprintf(syscall,"rm -f %s",outfilearr[row][col]);
//      system(syscall);
/*-------------- END COMMENTED -------------------------------*/
      free(outfilearr[row][col]);
      fclose(fp);

    }
    free(outfilearr[row]);
  }
  free(outfilearr);

  /* set the output pointers */
  *magptr=mag;
  *unwphaseptr=unwphase;

  /* free memory */
  free(inpline);

}


/* function: AdjustTileOffsets()
 * -----------------------------
 */
void AdjustTileOffsets(float **unwphase, long linelen, long nlines, 
		       long nrow, long ncol, long rowovrlp, long colovrlp, 
		       long ni, long nj){

  long row, col, i, j, ioff, joff, firstcol;
  long ilimlo, ilimhi, jlimlo, jlimhi, tempflow;
  long flowlimlo, flowlimhi, minflow, maxflow, iflow, nmax, nij, mstcostscale;
  long *flowhistogram;
  short **flows, **mstcosts, oldcornerflows[4];
  signed char **residue, **arcstatus;
  char switchcorner[4];
  double phaseoffset, rowphaseoffset;
  nodeT **nodes, ground[1], *source;
  bucketT *bkts;

  /* do nothing if we only have one tile */
  if(nrow<2 && ncol<2){
    return;
  }

  /* get memory */
  flows=(short **)Get2DRowColZeroMem(nrow,ncol,sizeof(short *),sizeof(short));
  flowlimhi=LARGESHORT/2;
  flowlimlo=-LARGESHORT/2;
  flowhistogram=CAlloc(flowlimhi-flowlimlo+1,sizeof(long));
  mstcosts=(short **)Get2DRowColMem(nrow,ncol,sizeof(short *),sizeof(short));
  mstcostscale=DEF_MSTCOSTSCALE;

  /* calculate row offsets between tiles to parse current flow */
  if(ni>nj){
    nij=ni;
  }else{
    nij=nj;
  }
  for(row=0;row<nrow-1;row++){
    i=(long )ceil(rowovrlp/2.0)+(row+1)*(ni-rowovrlp)-1;
    for(col=0;col<ncol;col++){
      joff=col*(nj-colovrlp);
      if(col!=0){
	jlimlo=joff+(long )ceil(colovrlp/2.0);
      }else{
	jlimlo=0;
      }
      if(col!=ncol-1){
	jlimhi=joff+(long )(nj-floor(colovrlp/2.0));
      }else{
	jlimhi=linelen;
      }

      /* build histogram of flows (phase differences) along tile edge */
      minflow=flowlimhi;
      maxflow=flowlimlo;
      for(j=jlimlo;j<jlimhi;j++){
	tempflow=-FRound((unwphase[i+1][j]-unwphase[i][j])/TWOPI);
	if(tempflow<minflow){
	  if(tempflow<flowlimlo){
	    tempflow=flowlimlo;
	  }
	  minflow=tempflow;
	}
	if(tempflow>maxflow){
	  if(tempflow>flowlimhi){
	    tempflow=flowlimhi;
	  }
	  maxflow=tempflow;
	}
	(flowhistogram[tempflow-flowlimlo])++;
      }

      /* set tile difference equal to most common phase flow (mode) */
      nmax=0;
      for(iflow=minflow;iflow<=maxflow;iflow++){
	if(flowhistogram[iflow-flowlimlo]>nmax){
	  nmax=flowhistogram[iflow-flowlimlo];
	  flows[row][col]=(short )iflow;
	}
	flowhistogram[iflow-flowlimlo]=0;
      }

      /* set tile arc cost based on histogram peak sharpness */
      mstcosts[row][col]=(short )ceil(mstcostscale
				      *(double )nmax/nij);

      /* check for overflow */
      if(flows[row][col]==flowlimhi || flows[row][col]==flowlimlo){
	fprintf(sp0,"short type overflow in assembling tiles.  abort.\n");
	exit(ABNORMAL_EXIT);
      }
    }
  }

  /* calculate column offsets between tiles to parse current flow */
  for(col=0;col<ncol-1;col++){
    j=(long )ceil(colovrlp/2.0)+(col+1)*(nj-colovrlp)-1;
    for(row=0;row<nrow;row++){
      ioff=row*(ni-rowovrlp);
      if(row!=0){
	ilimlo=ioff+(long )ceil(rowovrlp/2.0);
      }else{
	ilimlo=0;
      }
      if(row!=nrow-1){
	ilimhi=ioff+(long )(ni-floor(rowovrlp/2.0));;
      }else{
	ilimhi=nlines;
      }

      /* build histogram of flows (phase differences) along tile edge */
      minflow=flowlimhi;
      maxflow=flowlimlo;      
      for(i=ilimlo;i<ilimhi;i++){
	tempflow=FRound((unwphase[i][j+1]-unwphase[i][j])/TWOPI);
	if(tempflow<minflow){
	  if(tempflow<flowlimlo){
	    tempflow=flowlimlo;
	  }
	  minflow=tempflow;
	}
	if(tempflow>maxflow){
	  if(tempflow>flowlimhi){
	    tempflow=flowlimhi;
	  }
	  maxflow=tempflow;
	}
	(flowhistogram[tempflow-flowlimlo])++;
      }

      /* set tile difference equal to most common phase flow (mode) */
      nmax=0;
      for(iflow=minflow;iflow<=maxflow;iflow++){
	if(flowhistogram[iflow-flowlimlo]>nmax){
	  nmax=flowhistogram[iflow-flowlimlo];
	  flows[row+nrow-1][col]=(short )iflow;
	}
	flowhistogram[iflow-flowlimlo]=0;
      }

      /* set tile arc cost based on histogram peak sharpness */
      mstcosts[row+nrow-1][col]=(short )ceil(mstcostscale
					     *(double )nmax/nij);

      /* check for overflow */
      if(flows[row+nrow-1][col]==flowlimhi 
	 || flows[row+nrow-1][col]==flowlimlo){
	fprintf(sp0,"short type overflow in assembling tiles.  abort.\n");
	exit(ABNORMAL_EXIT);
      }
    }
  }


#ifdef DEBUG
  printf("Flow before tile adjustment\n");
  for(row=0;row<nrow;row++){
    printf("    ");
    for(col=0;col<ncol-1;col++){
      printf("%6d  ",flows[nrow-1+row][col]);
    }
    printf("\n");
    if(row<nrow-1){
      for(col=0;col<ncol;col++){
	printf("%6d  ",flows[row][col]);
      }
      printf("\n");
    }
  }
  printf("\n");

  printf("Costs\n");
  for(row=0;row<nrow;row++){
    printf("    ");
    for(col=0;col<ncol-1;col++){
      printf("%6d  ",mstcosts[nrow-1+row][col]);
    }
    printf("\n");
    if(row<nrow-1){
      for(col=0;col<ncol;col++){
	printf("%6d  ",mstcosts[row][col]);
      }
      printf("\n");
    }
  }
  printf("\n");
#endif


  /* get and initialize memory for nodes, buckets, and child array */
  nodes=(nodeT **)Get2DMem(nrow-1,ncol-1,sizeof(nodeT *),sizeof(nodeT));
  InitNodeNums(nrow,ncol,nodes,ground);

  /* calculate residues */
  source=NULL;
  residue=(signed char **)Get2DMem(nrow-1,ncol-1,sizeof(signed char *),
				   sizeof(signed char));
  for(row=0;row<nrow-1;row++){
    for(col=0;col<ncol-1;col++){
      residue[row][col]=flows[row][col]-flows[row][col+1]
	+flows[nrow-1+row][col]-flows[nrow+row][col];
      if(source==NULL && residue[row][col]){
	source=&(nodes[row][col]);
      }
    }
  }


#ifdef DEBUG
  printf("Residue:\n\n");
  for(row=0;row<nrow-1;row++){
    printf("    ");
    for(col=0;col<ncol-1;col++){
      printf("%6d",residue[row][col]);
    }
    printf("\n\n");
  }
  printf("\n");
#endif


  /* set up for and call the MST solver */
  if(source!=NULL){
  
    /* get memory for buckets, arc status, and flows for MST problem */
    bkts=(bucketT *)MAlloc(sizeof(bucketT));
    bkts->size=FRound((mstcostscale+1)*(nrow+ncol+1));
    bkts->bucketbase=(nodeT **)MAlloc(bkts->size*sizeof(nodeT *));
    bkts->minind=0;
    bkts->maxind=bkts->size-1;
    bkts->bucket=bkts->bucketbase;
    arcstatus=(signed char **)Get2DRowColMem(nrow,ncol,sizeof(signed char *),
					     sizeof(signed char));
     
    /* initialize data structures */
    InitNodes(nrow,ncol,nodes,ground);
    InitBuckets(bkts,source,bkts->size);
    
    /* SolveMST() takes row over column corner arcs, so use lesser of */
    /*   the arc costs and and switch flows back later if needed */
    for(i=0;i<4;i++){
      switchcorner[i]=FALSE;
    }
    if(mstcosts[nrow-1][0]<mstcosts[0][0]){
      mstcosts[0][0]=mstcosts[nrow-1][0];
      oldcornerflows[0]=flows[0][0];
      switchcorner[0]=TRUE;
    }
    if(mstcosts[nrow-1][ncol-2]<mstcosts[0][ncol-1]){
      mstcosts[0][ncol-1]=mstcosts[nrow-1][ncol-2];
      oldcornerflows[1]=flows[0][ncol-1];
      switchcorner[1]=TRUE;
    }
    if(mstcosts[2*nrow-2][0]<mstcosts[nrow-2][0]){
      mstcosts[nrow-2][0]=mstcosts[2*nrow-2][0];
      oldcornerflows[2]=flows[nrow-2][0];
      switchcorner[2]=TRUE;
    }
    if(mstcosts[2*nrow-2][ncol-2]<mstcosts[nrow-2][ncol-1]){
      mstcosts[nrow-2][ncol-1]=mstcosts[2*nrow-2][ncol-2];
      oldcornerflows[3]=flows[nrow-2][ncol-1];
      switchcorner[3]=TRUE;
    }

    /* call the MST solver to find a feasible flow */
    SolveMST(nodes,source,ground,bkts,mstcosts,residue,arcstatus,nrow,ncol);

    /* parse the MST solution */
    DischargeTree(source,mstcosts,flows,residue,arcstatus,
		  nodes,ground,nrow,ncol);

    /* switch corner flows if we switched costs above */
    if(switchcorner[0]){
      flows[nrow-1][0]+=(flows[0][0]-oldcornerflows[0]);
      flows[0][0]=oldcornerflows[0];
    }
    if(switchcorner[1]){
      flows[nrow-1][ncol-2]-=(flows[0][ncol-1]-oldcornerflows[1]);
      flows[0][ncol-1]=oldcornerflows[1];
    }
    if(switchcorner[2]){
      flows[2*nrow-2][0]-=(flows[nrow-2][0]-oldcornerflows[2]);
      flows[nrow-2][0]=oldcornerflows[2];
    }
    if(switchcorner[3]){
      flows[2*nrow-2][ncol-2]+=(flows[nrow-2][ncol-1]-oldcornerflows[3]);
      flows[nrow-2][ncol-1]=oldcornerflows[3];
    }

#ifdef DEBUG
  printf("arcstatus: (corner arcs switched)\n");
  for(row=0;row<nrow;row++){
    printf("    ");
    for(col=0;col<ncol-1;col++){
      printf("%6d  ",arcstatus[nrow-1+row][col]);
    }
    printf("\n");
    if(row<nrow-1){
      for(col=0;col<ncol;col++){
	printf("%6d  ",arcstatus[row][col]);
      }
      printf("\n");
    }
  }
  printf("\n");
#endif


    /* free intermediate memory */
    Free2DArray((void **)arcstatus,2*nrow-1);
    free(bkts->bucketbase);
    free(bkts);

  }

  /* correct unwrapped phase values based on flows */
  rowphaseoffset=0;
  for(row=0;row<nrow;row++){
    ioff=row*(ni-rowovrlp);
    if(row!=0){
      ilimlo=ioff+(long )ceil(rowovrlp/2.0);
    }else{
      ilimlo=0;
    }
    if(row!=nrow-1){
      ilimhi=ioff+(long )(ni-floor(rowovrlp/2.0));
    }else{
      ilimhi=nlines;
    }
    if(row>0){
      rowphaseoffset-=(flows[row-1][0]*TWOPI);
    }
    phaseoffset=rowphaseoffset;
    if(row>0){
      firstcol=0;
    }else{
      firstcol=1;
    }
    for(col=firstcol;col<ncol;col++){
      joff=col*(nj-colovrlp);
      if(col!=0){
	jlimlo=joff+(long )ceil(colovrlp/2.0);
      }else{
	jlimlo=0;
      }
      if(col!=ncol-1){
	jlimhi=joff+(long )(nj-floor(colovrlp/2.0));
      }else{
	jlimhi=linelen;
      }
      if(col>0){
	phaseoffset+=(flows[row+nrow-1][col-1]*TWOPI);
      }
      for(i=ilimlo;i<ilimhi;i++){
	for(j=jlimlo;j<jlimhi;j++){
	  unwphase[i][j]-=phaseoffset;
	}
      }
    }
  }


#ifdef DEBUG
  printf("Flow after tile adjustment\n");
  for(row=0;row<nrow;row++){
    printf("    ");
    for(col=0;col<ncol-1;col++){
      printf("%6d  ",flows[nrow-1+row][col]);
    }
    printf("\n");
    if(row<nrow-1){
      for(col=0;col<ncol;col++){
	printf("%6d  ",flows[row][col]);
      }
      printf("\n");
    }
  }
  printf("\n");
#endif


  /* free memory */
  Free2DArray((void **)mstcosts,2*nrow-1);
  Free2DArray((void **)nodes,nrow-1);
  Free2DArray((void **)residue,nrow-1);
  Free2DArray((void **)flows,2*nrow-1);
  free(flowhistogram);

}


/* function: CallSnaphu()
 * ----------------------
 */
#ifdef USE_PTHREADS
void *CallSnaphu(void *callsnaphuarg){

  long *iptr;
  long icall, ncalls;
  pthread_mutex_t *nextcallmutex;

  iptr=((callsnaphuargT *)callsnaphuarg)->iptr;
  nextcallmutex=((callsnaphuargT *)callsnaphuarg)->nextcallmutex;
  ncalls=((callsnaphuargT *)callsnaphuarg)->ncalls;

  while(TRUE){

    /* get the next snaphu call */
    if(pthread_mutex_lock(nextcallmutex)){
      fprintf(sp0,"pthread_mutex_lock() error\n");
      exit(ABNORMAL_EXIT);
    }
    icall=(*iptr)++;
    if(pthread_mutex_unlock(nextcallmutex)){
      fprintf(sp0,"pthread_mutex_unlock() error\n");
      exit(ABNORMAL_EXIT);
    }
    
    /* break if we've done all the calls */
    if(icall>=ncalls){
      break;
    }
    
    /* call snaphu */
    fprintf(sp1,((callsnaphuargT *)callsnaphuarg)->snaphucallarr[icall]);
    fprintf(sp1,"\n");
    fflush(sp1);
    if(system(((callsnaphuargT *)callsnaphuarg)->snaphucallarr[icall])){
      fprintf(sp0,"snaphu call failed.  failed call was\n");
      fprintf(sp0,((callsnaphuargT *)callsnaphuarg)->snaphucallarr[icall]);
      fprintf(sp0,"\nabort\n");
      exit(ABNORMAL_EXIT);
    }

  }
  return(NULL);
}
#endif


/* function: UnsetDumpFile()
 * -------------------------
 */
void UnsetDumpFile(char **dumpfile, char *printwarningptr){

  if(*dumpfile!=NULL){
    *printwarningptr=TRUE;
  }

}
