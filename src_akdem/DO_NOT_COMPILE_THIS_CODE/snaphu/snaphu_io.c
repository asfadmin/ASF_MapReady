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


/* function: SetDefaults()
 * -----------------------
 * Sets all parameters to their initial default values.  
 */
void SetDefaults(char *outfile, char *weightfile, char *ampfile, 
		 char *ampfile2, char *magfile, char *corrfile, 
		 char *estfile, paramT *params){

  /* files and optional arguments */
  StrNCopy(weightfile,DEF_WEIGHTFILE,MAXSTRLEN);
  StrNCopy(corrfile,DEF_CORRFILE,MAXSTRLEN);
  StrNCopy(ampfile,DEF_AMPFILE,MAXSTRLEN);
  StrNCopy(ampfile2,DEF_AMPFILE2,MAXSTRLEN);
  StrNCopy(outfile,DEF_OUTFILE,MAXSTRLEN);  
  StrNCopy(estfile,DEF_ESTFILE,MAXSTRLEN);  
  StrNCopy(magfile,DEF_MAGFILE,MAXSTRLEN);
  params->unwrapped=DEF_UNWRAPPED;
  params->eval=DEF_EVAL;
  params->initonly=DEF_INITONLY;
  params->initmethod=DEF_INITMETHOD;
  params->nostatcosts=DEF_NOSTATCOSTS;
  params->defo=DEF_DEFO;
  params->amplitude=DEF_AMPLITUDE;
  params->havemagnitude=DEF_HAVEMAGNITUDE;
  params->setedges=DEF_SETEDGES;
  params->verbose=DEF_VERBOSE;

  /* SAR and geometry parameters */
  params->bperp=DEF_BPERP;
  params->nlooks=DEF_NLOOKS;           
  params->nlooksrange=DEF_NLOOKSRANGE;
  params->nlooksaz=DEF_NLOOKSAZ;
  params->ncorrlooksrange=DEF_NCORRLOOKSRANGE;
  params->ncorrlooksaz=DEF_NCORRLOOKSAZ;
  params->theta=DEF_THETA;            
  params->nomrange=DEF_NOMRANGE;         
  params->dr=DEF_DR;               
  params->da=DEF_DA;               
  params->rangeres=DEF_RANGERES;         
  params->azres=DEF_AZRES;            
  params->lambda=DEF_LAMBDA;           

  /* scattering model parameters */
  params->kds=DEF_KDS;
  params->n=DEF_N;
  params->dzrcritfactor=DEF_DZRCRITFACTOR;
  params->shadow=DEF_SHADOW;
  params->dzeimin=DEF_DZEIMIN;
  params->laywidth=DEF_LAYWIDTH;
  params->layminei=DEF_LAYMINEI;
  params->sloperatiofactor=DEF_SLOPERATIOFACTOR;
  params->sigsqei=DEF_SIGSQEI;

  /* decorrelation model parameters */
  params->drho=DEF_DRHO;
  params->rhosconst1=DEF_RHOSCONST1;
  params->rhosconst2=DEF_RHOSCONST2;
  params->cstd1=DEF_CSTD1;
  params->cstd2=DEF_CSTD2;
  params->cstd3=DEF_CSTD3;
  params->defaultcorr=DEF_DEFAULTCORR;
  params->rhominfactor=DEF_RHOMINFACTOR;

  /* pdf model parameters */
  params->dzlaypeak=DEF_DZLAYPEAK;
  params->azdzfactor=DEF_AZDZFACTOR;
  params->dzeifactor=DEF_DZEIFACTOR;
  params->dzlayfactor=DEF_DZLAYFACTOR;
  params->layconst=DEF_LAYCONST;
  params->layfalloffconst=DEF_LAYFALLOFFCONST;
  params->sigsqshortmin=DEF_SIGSQSHORTMIN;
  params->sigsqlayfactor=DEF_SIGSQLAYFACTOR;
  
  /* deformation mode parameters */
  params->defoazdzfactor=DEF_DEFOAZDZFACTOR;
  params->defothreshfactor=DEF_DEFOTHRESHFACTOR;
  params->defomax=DEF_DEFOMAX;
  params->sigsqcorr=DEF_SIGSQCORR;
  params->defolayconst=DEF_DEFOLAYCONST;

  /* algorithm parameters */
  params->initmaxflow=DEF_INITMAXFLOW;
  params->arcmaxflowconst=DEF_ARCMAXFLOWCONST;
  params->maxflow=DEF_MAXFLOW;
  params->krowei=DEF_KROWEI;
  params->kcolei=DEF_KCOLEI;   
  params->kperpdpsi=DEF_KPERPDPSI;
  params->kpardpsi=DEF_KPARDPSI;
  params->threshold=DEF_THRESHOLD;  
  params->initdzr=DEF_INITDZR;    
  params->initdzstep=DEF_INITDZSTEP;    
  params->maxcost=DEF_MAXCOST;
  params->costscale=DEF_COSTSCALE;      
  params->costscalebperp=DEF_COSTSCALEBPERP;      
  params->srcrow=DEF_SRCROW;
  params->srccol=DEF_SRCCOL;
  params->p=DEF_P;
  params->nshortcycle=DEF_NSHORTCYCLE;
  params->maxnewnodeconst=DEF_MAXNEWNODECONST;
  params->maxcyclefraction=DEF_MAXCYCLEFRACTION;
  params->sourcemode=DEF_SOURCEMODE;
  params->maxnflowcycles=DEF_MAXNFLOWCYCLES;
  params->dumpall=DEF_DUMPALL;
  params->cs2scalefactor=DEF_CS2SCALEFACTOR;
  params->firstcol=DEF_FIRSTCOL;
  params->ntilecol=DEF_NTILECOL;
  params->firstrow=DEF_FIRSTROW;
  params->ntilerow=DEF_NTILEROW;
  params->tilesizedest=DEF_TILESIZEDEST;

  /* run option parameters */
  params->costinfile=NULL;
  params->costoutfile=NULL;

  /* file formats */
  params->infileformat=DEF_INFILEFORMAT;
  params->unwrappedinfileformat=DEF_UNWRAPPEDINFILEFORMAT;
  params->magfileformat=DEF_MAGFILEFORMAT;
  params->outfileformat=DEF_OUTFILEFORMAT;
  params->corrfileformat=DEF_CORRFILEFORMAT;
  params->estfileformat=DEF_ESTFILEFORMAT;
  params->ampfileformat=DEF_AMPFILEFORMAT;

  /* names of dump files */
  params->initfile=NULL;
  params->flowfile=NULL;
  params->eifile=NULL;
  params->rowcostfile=NULL;
  params->colcostfile=NULL;
  params->mstrowcostfile=NULL;
  params->mstcolcostfile=NULL;
  params->mstcostsfile=NULL;
  params->corrdumpfile=NULL;

}


/* function: ProcessArgs()
 * -----------------------
 * Parses command line inputs passed to main().
 */
void ProcessArgs(int argc, char *argv[], char *infile, long *linelenptr, 
		 char *outfile, char *weightfile, char *ampfile, 
		 char *ampfile2, char *magfile, char *corrfile, 
		 char *estfile, paramT *params){

  long i,j,k;
  signed char noarg_exit;

  /* required inputs */
  noarg_exit=FALSE;
  StrNCopy(infile,"",MAXSTRLEN);
  *linelenptr=0;

  /* loop over inputs */
  if(argc<2){                             /* catch zero arguments in */
    fprintf(sp0,OPTIONSHELPBRIEF);
    exit(ABNORMAL_EXIT);
  }
  for(i=1;i<argc;i++){                  
    /* if argument is an option */
    if(argv[i][0]=='-'){   
      if(strlen(argv[i])==1){
	fprintf(sp0,"invalid command line argument -\n");
	exit(ABNORMAL_EXIT);
      }else if(argv[i][1]!='-'){
	for(j=1;j<strlen(argv[i]);j++){
	  if(argv[i][j]=='h'){
	    fprintf(sp0,OPTIONSHELPFULL);
	    exit(ABNORMAL_EXIT);
	  }else if(argv[i][j]=='u'){
	    params->unwrapped=TRUE;
	  }else if(argv[i][j]=='s'){
	    params->setedges=TRUE;
	  }else if(argv[i][j]=='d'){
	    params->defo=TRUE;
	  }else if(argv[i][j]=='q'){
	    params->eval=TRUE;
	    params->unwrapped=TRUE;
	  }else if(argv[i][j]=='f'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      
	      /* read user-supplied configuration file */
	      ReadConfigFile(argv[i],infile,outfile,weightfile,
			     ampfile,ampfile2,magfile,corrfile,estfile,
			     linelenptr,params);
	      
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='o'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(outfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='c'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(corrfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='m'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(magfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='a'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(ampfile,argv[i],MAXSTRLEN);
	      params->amplitude=TRUE;
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='A'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(ampfile,argv[i],MAXSTRLEN);
	      params->amplitude=FALSE;
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='e'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(estfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='E'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      params->tilesizedest=TRUE;
	      StrNCopy(estfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='w'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      StrNCopy(weightfile,argv[i],MAXSTRLEN);
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='b'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      if(StringToDouble(argv[i],&(params->bperp)) || !(params->bperp)){
		fprintf(sp0,"option -%c requires non-zero decimal argument\n",
			argv[i-1][j]);
		exit(ABNORMAL_EXIT);
	      }
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='p'){
	    if(++i<argc && j==strlen(argv[i-1])-1){
	      if(StringToDouble(argv[i],&(params->p))){
		fprintf(sp0,"option -%c requires decimal argument\n",
			argv[i-1][j]);
		exit(ABNORMAL_EXIT);
	      }
	      break;
	    }else{
	      noarg_exit=TRUE;
	    }
	  }else if(argv[i][j]=='i'){
	    params->initonly=TRUE;
	  }else if(argv[i][j]=='n'){
	    params->nostatcosts=TRUE;
	  }else if(argv[i][j]=='v'){
	    params->verbose=TRUE;
	  }else{
	    fprintf(sp0,"unrecognized option -%c\n",argv[i][j]);
	    exit(ABNORMAL_EXIT);
	  }
	  if(noarg_exit){
	    fprintf(sp0,"option -%c requires an argument\n",argv[i-1][j]);
	    exit(ABNORMAL_EXIT);
	  }
	}
      }else{
	/* argument is a "--" option */
	if(!strcmp(argv[i],"--costinfile")){
	  if(++i<argc){
	    params->costinfile=(char *)MAlloc(MAXSTRLEN);
	    StrNCopy(params->costinfile,argv[i],MAXSTRLEN);
	  }else{
	    noarg_exit=TRUE;
	  }
	}else if(!strcmp(argv[i],"--costoutfile")){
	  if(++i<argc){
	    params->costoutfile=(char *)MAlloc(MAXSTRLEN);
	    StrNCopy(params->costoutfile,argv[i],MAXSTRLEN);
	  }else{
	    noarg_exit=TRUE;
	  }
	}else if(!strcmp(argv[i],"--debug") || !strcmp(argv[i],"--dumpall")){
	  params->dumpall=TRUE;
	}else if(!strcmp(argv[i],"--mst")){
	  params->initmethod=MSTINIT;
	}else if(!strcmp(argv[i],"--mcf")){
	  params->initmethod=MCFINIT;
	}else if(!strcmp(argv[i],"--aa")){
	  if(i+2<argc){
	    StrNCopy(ampfile,argv[++i],MAXSTRLEN);
	    StrNCopy(ampfile2,argv[++i],MAXSTRLEN);
	    params->ampfileformat=FLOAT_DATA;
	    params->amplitude=TRUE;
	  }else{
	    noarg_exit=TRUE;
	  }
	}else if(!strcmp(argv[i],"--AA")){
	  if(++i+1<argc){
	    StrNCopy(ampfile,argv[i++],MAXSTRLEN);
	    StrNCopy(ampfile2,argv[i],MAXSTRLEN);
	    params->ampfileformat=FLOAT_DATA;
	    params->amplitude=FALSE;
	  }else{
	    noarg_exit=TRUE;
	  }
	}else if(!strcmp(argv[i],"--tile")){
	  if(++i+3<argc){
	    if(StringToLong(argv[i++],&(params->firstcol))
	       || StringToLong(argv[i++],&(params->firstrow))
	       || StringToLong(argv[i++],&(params->ntilecol))
	       || StringToLong(argv[i],&(params->ntilerow))){
	      fprintf(sp0,"option %s requires four integer arguments\n",
		      argv[i-4]);
	      exit(ABNORMAL_EXIT);
	    }
	  }else{
	    noarg_exit=TRUE;
	  }
	}else if(!strcmp(argv[i],"--help")){
	  fprintf(sp0,OPTIONSHELPFULL);
	  exit(ABNORMAL_EXIT);	  
	}else{
	  fprintf(sp0,"unrecognized option %s\n",argv[i]);
	  exit(ABNORMAL_EXIT);
	}
	if(noarg_exit){
	  fprintf(sp0,"incorrect number of arguments for option %s\n",
		  argv[i-1]);
	  exit(ABNORMAL_EXIT);
	}
      }
    }else{                                
      /* argument is not an option */
      if(!strlen(infile)){
        StrNCopy(infile,argv[i],MAXSTRLEN);
      }else if(*linelenptr==0){
	for(k=0;k<strlen(argv[i]);k++){
	  if(!isdigit(argv[i][k])){
	    fprintf(sp0,"line length must be positive integer\n");
	    exit(ABNORMAL_EXIT);
	  }
	}
	if(StringToLong(argv[i],linelenptr) || *linelenptr<=0){
	  fprintf(sp0,"line length must be positive integer\n");
	  exit(ABNORMAL_EXIT);
	}	  
      }else{
        fprintf(sp0,"multiple input files: %s and %s\n",infile,argv[i]);
	exit(ABNORMAL_EXIT);
      }
    }
  } /* end for loop over arguments */

  /* check to make sure we have required arguments */
  if(!strlen(infile) || !(*linelenptr)){
    fprintf(sp0,"not enough input arguments.  type %s -h for help\n",
	    PROGRAMNAME);
    exit(ABNORMAL_EXIT);
  }

} /* end of ProcessArgs */


/* function: CheckParams()
 * -----------------------
 * Checks all parameters to make sure they are valid.  This is just a boring
 * function with lots of checks in it.
 */
void CheckParams(char *infile, char *outfile, char *weightfile, 
		 char *ampfile, char *ampfile2, char *magfile, 
		 char *corrfile, long linelen, long nlines, paramT *params){

  FILE *fp;


  /* make sure output file is writable (try opening in append mode) */
  /* file will be opened in write mode later, clobbering existing file */
  if((fp=fopen(outfile,"a"))==NULL){
    fprintf(sp0,"unable to write to file %s\n",outfile);
    exit(ABNORMAL_EXIT);
  }else{
    if(ftell(fp)){
      fclose(fp);
    }else{
      fclose(fp);
      remove(outfile);
    }
  }

  /* make sure options aren't contradictory */
  if(params->initonly && params->unwrapped){
    fprintf(sp0,"cannot use initialize-only mode with unwrapped input\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->initonly && params->p>=0){
    fprintf(sp0,"cannot use initialize-only mode with Lp costs\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->nostatcosts && !(params->initonly || params->p>=0)){
    fprintf(sp0,"no-statistical-costs option can only be used in\n");
    fprintf(sp0,"  initialize-only or Lp-norm modes\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->nostatcosts && params->defo){
    fprintf(sp0,"no-statistical-costs option cannot be used\n");
    fprintf(sp0,"  in deformation-cost mode\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->costinfile && params->nostatcosts){
    fprintf(sp0,"no-statistical-costs option cannot be given\n");
    fprintf(sp0,"  if input cost file is specified\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->costoutfile && params->nostatcosts){
    fprintf(sp0,"no-statistical-costs option cannot be given\n");
    fprintf(sp0,"  if output cost file is specified\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->setedges && !params->unwrapped){
    fprintf(sp0,"set-edges option can only be used with unwrapped input\n");
    exit(ABNORMAL_EXIT);
  }
  /*
  if(params->setedges && params->sourcemode){
    fprintf(sp0,"set-edges option can only be used with ground as source\n");
    params->sourcemode=0;
  }
  */

  /* check geometry parameters */
  if(params->nlooks<=0){
    fprintf(sp0,"number of looks nlooks must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->theta<=0 || params->theta>=PI/2){
    fprintf(sp0,"look angle theta must be between 0 and 90 deg (pi/2 rad)\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->nomrange<=0){
    fprintf(sp0,"nominal range nomrange must be positive (meters)\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->dr<=0 || params->da<=0){
    fprintf(sp0,"pixel spacings dr and da must be positive (meters)\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->rangeres<=0 || params->azres<=0){
    fprintf(sp0,"resolutions parameters must be positive (meters)\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->lambda<=0){
    fprintf(sp0,"wavelength lambda  must be positive (meters)\n");
    exit(ABNORMAL_EXIT);
  }

  /* check scattering model defaults */
  if(params->kds<=0){
    fprintf(sp0,"scattering model parameter kds must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->n<=0){
    fprintf(sp0,"scattering model parameter n must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->dzrcritfactor<0){
    fprintf(sp0,"dzrcritfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->laywidth<1){
    fprintf(sp0,"layover window width laywidth must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->layminei<0){
    fprintf(sp0,"layover minimum brightness must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sloperatiofactor<0){
    fprintf(sp0,"slope ratio fudge factor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sigsqei<=0){
    fprintf(sp0,"intensity estimate variance must be positive\n");
    exit(ABNORMAL_EXIT);
  }

  /* check decorrelation model defaults */
  if(params->drho<=0){
    fprintf(sp0,"correlation step size drho must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->rhosconst1<=0 || params->rhosconst2<=0){
    fprintf(sp0,"parameters rhosconst1 and rhosconst2 must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(!strlen(corrfile) && (params->defaultcorr<0 || params->defaultcorr>1)){
    fprintf(sp0,"default correlation must be between 0 and 1\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->rhominfactor<0){
    fprintf(sp0,"parameter rhominfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->ncorrlooksaz<=0 || params->ncorrlooksrange<=0
     || params->nlooksaz<=0 || params->nlooksrange<=0){
    fprintf(sp0,"numbers of looks must be positive integer\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->ncorrlooksaz<params->nlooksaz){ 
    fprintf(sp0,"NCORRLOOKSAZ cannot be smaller than NLOOKSAZ\n");
    fprintf(sp0,"  setting NCORRLOOKSAZ to equal NLOOKSAZ\n");
    params->ncorrlooksaz=params->nlooksaz;
  }
  if(params->ncorrlooksrange<params->nlooksrange){ 
    fprintf(sp0,"NCORRLOOKSRANGE cannot be smaller than NLOOKSRANGE\n");
    fprintf(sp0,"  setting NCORRLOOKSRANGE to equal NLOOKSRANGE\n");
    params->ncorrlooksrange=params->nlooksrange;
  }
    
  /* check pdf model parameters */
  if(params->azdzfactor<0){
    fprintf(sp0,"parameter azdzfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->dzeifactor<0){
    fprintf(sp0,"parameter dzeifactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->dzlayfactor<0){
    fprintf(sp0,"parameter dzlayfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->layconst<=0){
    fprintf(sp0,"parameter layconst must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->layfalloffconst<0){
    fprintf(sp0,"parameter layfalloffconst must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sigsqshortmin<=0){
    fprintf(sp0,"parameter sigsqshortmin must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sigsqlayfactor<0){
    fprintf(sp0,"parameter sigsqlayfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }

  /* check deformation mode parameters */
  if(params->defoazdzfactor<0){
    fprintf(sp0,"parameter defoazdzfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->defothreshfactor<0){
    fprintf(sp0,"parameter defothreshfactor must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->defomax<0){
    fprintf(sp0,"parameter defomax must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sigsqcorr<0){
    fprintf(sp0,"parameter sigsqcorr must be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->defolayconst<=0){
    fprintf(sp0,"parameter defolayconst must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  
  /* check algorithm parameters */
  /* be sure to check for things that will cause type overflow */
  /* or floating point exception */
  if((params->initmaxflow)<1 && (params->initmaxflow)!=AUTOCALCSTATMAX){
    fprintf(sp0,"initialization maximum flow must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if((params->arcmaxflowconst)<1){
    fprintf(sp0,"arcmaxflowconst must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if((params->maxflow)<1){
    fprintf(sp0,"maxflow must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->krowei<=0 || params->kcolei<=0){
    fprintf(sp0,"averaging window sizes krowei and kcolei must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->kperpdpsi<=0 || params->kpardpsi<=0){
    fprintf(sp0,
	  "averaging window sizes kperpdpsi and kpardpsi must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->threshold<=0){
    fprintf(sp0,"numerical solver threshold must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->initdzr<=0){
    fprintf(sp0,"initdzr must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->initdzstep<=0){
    fprintf(sp0,"initdzstep must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->maxcost>POSSHORTRANGE || params->maxcost<=0){
    fprintf(sp0,"maxcost must be positive and within range or short int\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->costscale<=0){
    fprintf(sp0,"cost scale factor costscale must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->costscalebperp<=0){
    fprintf(sp0,"cost scale normalization baseline must be positive\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->p<0 && params->p!=PROBCOSTP){
    fprintf(sp0,"Lp-norm parameter p should be nonnegative\n");
    exit(ABNORMAL_EXIT);
  }
  if((!params->defo && params->maxflow*params->nshortcycle)>POSSHORTRANGE){
    fprintf(sp0,"maxflow exceeds range of short int for given nshortcycle\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->defo && ceil(params->defomax*params->nshortcycle)>POSSHORTRANGE){
    fprintf(sp0,"defomax exceeds range of short int for given nshortcycle\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->maxnewnodeconst<=0 || params->maxnewnodeconst>1){
    fprintf(sp0,"maxnewnodeconst must be between 0 and 1\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->sourcemode>1 || params->sourcemode<-1){
    fprintf(sp0,"sourcemode must be -1, 0, or 1\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->firstrow<1 || params->firstcol<1 
     || params->firstcol+params->ntilecol-1>linelen
     || params->firstrow+params->ntilerow-1>nlines
     || params->ntilecol<1 || params->ntilerow<1){
    fprintf(sp0,"illegal tile indices\n");
    exit(ABNORMAL_EXIT);
  }

}


/* function: ReadConfigFile()
 * --------------------------
 * Read in parameter values from a file, overriding existing parameters.
 */
void ReadConfigFile(char *conffile, char *infile, char *outfile, 
		    char *weightfile, char *ampfile, char *ampfile2, 
		    char *magfile, char *corrfile, char *estfile, 
		    long *linelenptr, paramT *params){
  
  long nlines, nparams, nfields;
  FILE *fp;
  char buf[MAXLINELEN];
  char str1[MAXLINELEN], str2[MAXLINELEN];
  char *ptr;
  signed char badparam;

  /* open input config file */
  if(strlen(conffile)){
    if((fp=fopen(conffile,"r"))==NULL){

      /* abort if we were given a non-zero length name that is unreadable */
      fprintf(sp0,"unable to read configuration file %s\n",conffile);
      exit(ABNORMAL_EXIT);
    }
  }else{
    
    /* if we were given a zero-length name, just ignore it and go on */
    return;
  }

  /* read each line and convert the first two fields */
  nlines=0;
  nparams=0;
  while(TRUE){

    /* read a line from the file and store it in buffer buf */
    buf[0]='\0';
    ptr=fgets(buf,MAXLINELEN,fp);

    /* break when we read EOF without reading any text */
    if(ptr==NULL && !strlen(buf)){
      break;
    }
    nlines++;

    /* make sure we got the whole line */
    if(strlen(buf)>=MAXLINELEN-1){
      fprintf(sp0,"line %ld in file %s exceeds maximum line length.  Abort.\n",
	      nlines,conffile);
      exit(ABNORMAL_EXIT);
    }
      
    /* read the first two fields */
    /* (str1, str2 same size as buf, so can't overflow them */
    nfields=sscanf(buf,"%s %s",str1,str2);

    /* if only one field is read, and it is not a comment, we have an error */
    if(nfields==1 && isalnum(str1[0])){
      fprintf(sp0,"unrecognized configuration parameter '%s' (%s:%ld)\n",
	      str1,conffile,nlines);
      exit(ABNORMAL_EXIT);
    }

    /* if we have (at least) two non-comment fields */
    if(nfields==2 && isalnum(str1[0])){

      /* do the conversions */
      nparams++;
      badparam=FALSE;
      if(!strcmp(str1,"INFILE")){
	StrNCopy(infile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"OUTFILE")){
	StrNCopy(outfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"WEIGHTFILE")){
	StrNCopy(weightfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"AMPFILE") || !strcmp(str1,"AMPFILE1")){
	if(strlen(ampfile2) && !params->amplitude){
	  fprintf(sp0,"Cannot specify both amplitude and power.  Abort\n");
	  exit(ABNORMAL_EXIT);
	}
	StrNCopy(ampfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"AMPFILE2")){
	if(strlen(ampfile) && !params->amplitude){
	  fprintf(sp0,"Cannot specify both amplitude and power.  Abort\n");
	  exit(ABNORMAL_EXIT);
	}
	StrNCopy(ampfile2,str2,MAXSTRLEN);
	params->ampfileformat=FLOAT_DATA;
      }else if(!strcmp(str1,"PWRFILE") || !strcmp(str1,"PWRFILE1")){
	if(strlen(ampfile2) && params->amplitude){
	  fprintf(sp0,"Cannot specify both amplitude and power.  Abort\n");
	  exit(ABNORMAL_EXIT);
	}	
	StrNCopy(ampfile,str2,MAXSTRLEN);
	params->amplitude=FALSE;
      }else if(!strcmp(str1,"PWRFILE2")){
	if(strlen(ampfile) && params->amplitude){
	  fprintf(sp0,"Cannot specify both amplitude and power.  Abort\n");
	  exit(ABNORMAL_EXIT);
	}	
	StrNCopy(ampfile2,str2,MAXSTRLEN);
	params->amplitude=FALSE;
	params->ampfileformat=FLOAT_DATA;
      }else if(!strcmp(str1,"MAGFILE")){
	StrNCopy(magfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"CORRFILE")){
	StrNCopy(corrfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"ESTIMATEFILE")){
	StrNCopy(estfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"LINELENGTH")){
	badparam=StringToLong(str2,linelenptr);
      }else if(!strcmp(str1,"STATCOSTMODE")){
	if(!strcmp(str2,"TOPO")){
	  params->defo=FALSE;
	}else if(!strcmp(str2,"DEFO")){
	  params->defo=TRUE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"INITONLY")){
	if(IsTrue(str2)){
	  params->initonly=TRUE;
	}else if(IsFalse(str2)){
	  params->initonly=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"NOSTATCOSTS")){
	if(IsTrue(str2)){
	  params->nostatcosts=TRUE;
	}else if(IsFalse(str2)){
	  params->nostatcosts=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"UNWRAPPED_IN")){
	if(IsTrue(str2)){
	  params->unwrapped=TRUE;
	}else if(IsFalse(str2)){
	  params->unwrapped=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"DEBUG") || !strcmp(str1,"DUMPALL")){
	if(IsTrue(str2)){
	  params->dumpall=TRUE;
	}else if(IsFalse(str2)){
	  params->dumpall=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"VERBOSE")){
	if(IsTrue(str2)){
	  params->verbose=TRUE;
	}else if(IsFalse(str2)){
	  params->verbose=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"SETEDGES")){
	if(IsTrue(str2)){
	  params->setedges=TRUE;
	}else if(IsFalse(str2)){
	  params->setedges=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"INITMETHOD")){
	if(!strcmp(str2,"MST") || !strcmp(str2,"mst")){
	  params->initmethod=MSTINIT;
	}else if(!strcmp(str2,"MCF") || !strcmp(str2,"mcf") 
		 || !strcmp(str2,"CS2") || !strcmp(str2,"cs2")){
	  params->initmethod=MCFINIT;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"BPERP")){
	badparam=StringToDouble(str2,&(params->bperp));
      }else if(!strcmp(str1,"NLOOKS")){
	badparam=StringToLong(str2,&(params->nlooks));
      }else if(!strcmp(str1,"NLOOKSRANGE")){
	badparam=StringToLong(str2,&(params->nlooksrange));
      }else if(!strcmp(str1,"NLOOKSAZ")){
	badparam=StringToLong(str2,&(params->nlooksaz));
      }else if(!strcmp(str1,"NCORRLOOKSRANGE")){
	badparam=StringToLong(str2,&(params->ncorrlooksrange));
      }else if(!strcmp(str1,"NCORRLOOKSAZ")){
	badparam=StringToLong(str2,&(params->ncorrlooksaz));
      }else if(!strcmp(str1,"THETA_DEG")){
	if(!(badparam=StringToDouble(str2,&(params->theta)))){
	  (params->theta)*=(PI/180.0);
	}
      }else if(!strcmp(str1,"THETA_RAD")){
	badparam=StringToDouble(str2,&(params->theta));
      }else if(!strcmp(str1,"NOMRANGE")){
	badparam=StringToDouble(str2,&(params->nomrange));
      }else if(!strcmp(str1,"DR")){
	badparam=StringToDouble(str2,&(params->dr));
      }else if(!strcmp(str1,"DA")){
	badparam=StringToDouble(str2,&(params->da));
      }else if(!strcmp(str1,"RANGERES")){
	badparam=StringToDouble(str2,&(params->rangeres));
      }else if(!strcmp(str1,"AZRES")){
	badparam=StringToDouble(str2,&(params->azres));
      }else if(!strcmp(str1,"LAMBDA")){
	badparam=StringToDouble(str2,&(params->lambda));
      }else if(!strcmp(str1,"KDS") || !strcmp(str1,"KSD")){
	if(!strcmp(str1,"KSD")){
	  fprintf(sp0,"WARNING: parameter KSD interpreted as KDS (%s:%ld)\n",
		  conffile,nlines);
	}
	badparam=StringToDouble(str2,&(params->kds));
      }else if(!strcmp(str1,"N")){
	badparam=StringToDouble(str2,&(params->n));
      }else if(!strcmp(str1,"DZRCRITFACTOR")){
	badparam=StringToDouble(str2,&(params->dzrcritfactor));
      }else if(!strcmp(str1,"SHADOW")){
	if(IsTrue(str2)){
	  params->shadow=TRUE;
	}else if(IsFalse(str2)){
	  params->shadow=FALSE;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"DZEIMIN")){
	badparam=StringToDouble(str2,&(params->dzeimin));
      }else if(!strcmp(str1,"LAYWIDTH")){
	badparam=StringToLong(str2,&(params->laywidth));
      }else if(!strcmp(str1,"LAYMINEI")){
	badparam=StringToDouble(str2,&(params->layminei));
      }else if(!strcmp(str1,"SLOPERATIOFACTOR")){
	badparam=StringToDouble(str2,&(params->sloperatiofactor));
      }else if(!strcmp(str1,"SIGSQEI")){
	badparam=StringToDouble(str2,&(params->sigsqei));
      }else if(!strcmp(str1,"DRHO")){
	badparam=StringToDouble(str2,&(params->drho));
      }else if(!strcmp(str1,"RHOSCONST1")){
	badparam=StringToDouble(str2,&(params->rhosconst1));
      }else if(!strcmp(str1,"RHOSCONST2")){
	badparam=StringToDouble(str2,&(params->rhosconst2));
      }else if(!strcmp(str1,"CSTD1")){
	badparam=StringToDouble(str2,&(params->cstd1));
      }else if(!strcmp(str1,"CSTD2")){
	badparam=StringToDouble(str2,&(params->cstd2));
      }else if(!strcmp(str1,"CSTD3")){
	badparam=StringToDouble(str2,&(params->cstd3));
      }else if(!strcmp(str1,"DEFAULTCORR")){
	badparam=StringToDouble(str2,&(params->defaultcorr));
      }else if(!strcmp(str1,"RHOMINFACTOR")){
	badparam=StringToDouble(str2,&(params->rhominfactor));
      }else if(!strcmp(str1,"DZLAYPEAK")){
	badparam=StringToDouble(str2,&(params->dzlaypeak));
      }else if(!strcmp(str1,"AZDZFACTOR")){
	badparam=StringToDouble(str2,&(params->azdzfactor));
      }else if(!strcmp(str1,"DZEIFACTOR")){
	badparam=StringToDouble(str2,&(params->dzeifactor));
      }else if(!strcmp(str1,"DZLAYFACTOR")){
	badparam=StringToDouble(str2,&(params->dzlayfactor));
      }else if(!strcmp(str1,"LAYCONST")){
	badparam=StringToDouble(str2,&(params->layconst));
      }else if(!strcmp(str1,"LAYFALLOFFCONST")){
	badparam=StringToDouble(str2,&(params->layfalloffconst));
      }else if(!strcmp(str1,"SIGSQSHORTMIN")){
	badparam=StringToLong(str2,&(params->sigsqshortmin));
      }else if(!strcmp(str1,"SIGSQLAYFACTOR")){
	badparam=StringToDouble(str2,&(params->sigsqlayfactor));
      }else if(!strcmp(str1,"DEFOAZDZFACTOR")){
	badparam=StringToDouble(str2,&(params->defoazdzfactor));
      }else if(!strcmp(str1,"DEFOTHRESHFACTOR")){
	badparam=StringToDouble(str2,&(params->defothreshfactor));
      }else if(!strcmp(str1,"DEFOMAX_CYCLE")){
	badparam=StringToDouble(str2,&(params->defomax));
      }else if(!strcmp(str1,"DEFOMAX_RAD")){
	if(!(badparam=StringToDouble(str2,&(params->defomax)))){
	  params->defomax/=TWOPI;
	}
      }else if(!strcmp(str1,"SIGSQCORR")){
	badparam=StringToDouble(str2,&(params->sigsqcorr));
      }else if(!strcmp(str1,"DEFOLAYCONST") || !strcmp(str1,"DEFOCONST")){
	badparam=StringToDouble(str2,&(params->defolayconst));
      }else if(!strcmp(str1,"INITMAXFLOW")){
	badparam=StringToLong(str2,&(params->initmaxflow));
      }else if(!strcmp(str1,"ARCMAXFLOWCONST")){
	badparam=StringToLong(str2,&(params->arcmaxflowconst));
      }else if(!strcmp(str1,"MAXFLOW")){
	badparam=StringToLong(str2,&(params->maxflow));
      }else if(!strcmp(str1,"KROWEI") || !strcmp(str1,"KROW")){
	badparam=StringToLong(str2,&(params->krowei));
      }else if(!strcmp(str1,"KCOLEI") || !strcmp(str1,"KCOL")){
	badparam=StringToLong(str2,&(params->kcolei));
      }else if(!strcmp(str1,"KPERPDPSI")){
	badparam=StringToLong(str2,&(params->kperpdpsi));
      }else if(!strcmp(str1,"KPARDPSI")){
	badparam=StringToLong(str2,&(params->kpardpsi));
      }else if(!strcmp(str1,"THRESHOLD")){
	badparam=StringToDouble(str2,&(params->threshold));
      }else if(!strcmp(str1,"INITDZR")){
	badparam=StringToDouble(str2,&(params->initdzr));
      }else if(!strcmp(str1,"INITDZSTEP")){
	badparam=StringToDouble(str2,&(params->initdzstep));
      }else if(!strcmp(str1,"MAXCOST")){
	badparam=StringToDouble(str2,&(params->maxcost));
      }else if(!strcmp(str1,"COSTSCALE")){
	badparam=StringToDouble(str2,&(params->costscale));
      }else if(!strcmp(str1,"COSTSCALEBPERP")){
	badparam=StringToDouble(str2,&(params->costscalebperp));
      }else if(!strcmp(str1,"CS2SCALEFACTOR")){
	badparam=StringToLong(str2,&(params->cs2scalefactor));
      }else if(!strcmp(str1,"NSHORTCYCLE")){
	badparam=StringToLong(str2,&(params->nshortcycle));
      }else if(!strcmp(str1,"MAXNEWNODECONST")){
	badparam=StringToDouble(str2,&(params->maxnewnodeconst));
      }else if(!strcmp(str1,"MAXNFLOWCYCLES")){
	badparam=StringToLong(str2,&(params->maxnflowcycles));
      }else if(!strcmp(str1,"MAXCYCLEFRACTION")){
	badparam=StringToDouble(str2,&(params->maxcyclefraction));
	params->maxnflowcycles=USEMAXCYCLEFRACTION;
      }else if(!strcmp(str1,"SOURCEMODE")){
	badparam=StringToLong(str2,&(params->sourcemode));
      }else if(!strcmp(str1,"COSTINFILE")){
	params->costinfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->costinfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"COSTOUTFILE")){
	params->costoutfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->costoutfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"INFILEFORMAT")){
	if(!strcmp(str2,"COMPLEX_DATA")){
	  params->infileformat=COMPLEX_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->infileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"UNWRAPPEDINFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->unwrappedinfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->unwrappedinfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->unwrappedinfileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"MAGFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->magfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->magfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->magfileformat=FLOAT_DATA;
	}else if(!strcmp(str2,"COMPLEX_DATA")){
	  params->magfileformat=COMPLEX_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"OUTFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->outfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->outfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->outfileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"CORRFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->corrfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->corrfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->corrfileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"AMPFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->ampfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->ampfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->ampfileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"ESTFILEFORMAT")){
	if(!strcmp(str2,"ALT_LINE_DATA")){
	  params->estfileformat=ALT_LINE_DATA;
	}else if(!strcmp(str2,"ALT_SAMPLE_DATA")){
	  params->estfileformat=ALT_SAMPLE_DATA;
	}else if(!strcmp(str2,"FLOAT_DATA")){
	  params->estfileformat=FLOAT_DATA;
	}else{
	  badparam=TRUE;
	}
      }else if(!strcmp(str1,"TILESIZEDEST")){
	if(IsTrue(str2)){
	  params->tilesizedest=TRUE;
	}else if(IsFalse(str2)){
	  params->tilesizedest=FALSE;
	}else{
	  badparam=TRUE;
	}	
      }else if(!strcmp(str1,"INITFILE")){
	params->initfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->initfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"FLOWFILE")){
	params->flowfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->flowfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"EIFILE")){
	params->eifile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->eifile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"ROWCOSTFILE")){
	params->rowcostfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->rowcostfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"COLCOSTFILE")){
	params->colcostfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->colcostfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"MSTROWCOSTFILE")){
	params->mstrowcostfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->mstrowcostfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"MSTCOLCOSTFILE")){
	params->mstcolcostfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->mstcolcostfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"MSTCOSTSFILE")){
	params->mstcostsfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->mstcostsfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"CORRDUMPFILE")){
	params->corrdumpfile=MAlloc(MAXSTRLEN*sizeof(char));
	StrNCopy(params->corrdumpfile,str2,MAXSTRLEN);
      }else if(!strcmp(str1,"FIRSTCOL")){
	badparam=StringToLong(str2,&(params->firstcol));
      }else if(!strcmp(str1,"FIRSTROW")){
	badparam=StringToLong(str2,&(params->firstrow));
      }else if(!strcmp(str1,"NTILECOL")){
	badparam=StringToLong(str2,&(params->ntilecol));
      }else if(!strcmp(str1,"NTILEROW")){
	badparam=StringToLong(str2,&(params->ntilerow));
      }else{
	fprintf(sp0,"unrecognized configuration parameter '%s' (%s:%ld)\n",
		str1,conffile,nlines);
	exit(ABNORMAL_EXIT);
      }

      /* give an error if we had trouble interpreting the line */
      if(badparam){
	fprintf(sp0,"illegal argument %s for parameter %s (%s:%ld)\n",
		str2,str1,conffile,nlines);
	exit(ABNORMAL_EXIT);
      }

    }
  }

  /* finish up */
  fclose(fp);
  fprintf(sp1,"%ld parameters input from file %s (%ld lines total)\n",
	  nparams,conffile,nlines);

}


/* function: SetTileParams() 
 * -------------------------
 * Sets the input file parameters for file reads if we are in tile mode,
 * or just sets the number of rows and columns if not.
 */
void SetTileParams(char *infile, long linelen, long *nlinesptr, 
		   long *nrowptr, long *ncolptr, paramT *params){

  FILE *fp;
  long filesize, datasize;


  /* get size of input file in rows and columns */
  if((fp=fopen(infile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",infile);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_END);
  filesize=ftell(fp);
  fclose(fp);
  if(params->infileformat==FLOAT_DATA){
    datasize=sizeof(float);
  }else{
    datasize=2*sizeof(float);
  }
  if(filesize % (datasize*linelen)){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    infile);
    exit(ABNORMAL_EXIT);
  }
  *nlinesptr=filesize/(datasize*linelen);               /* implicit floor */

  /* set number of rows and columns of good data to process */
  if(!params->ntilecol){
    params->ntilecol=linelen;
  }
  *ncolptr=params->ntilecol;
  if(!params->ntilerow){
    params->ntilerow=*nlinesptr;
  }
  *nrowptr=params->ntilerow;


}


/* function: WriteOutputFile()
 * ---------------------------
 * Writes the unwrapped phase to the output file specified, in the
 * format given in the parameter structure.
 */
void WriteOutputFile(float **mag, float **unwrappedphase, char *outfile,
		     long nrow, long ncol, paramT *params){

  if(params->outfileformat==ALT_LINE_DATA){
    WriteAltLineFile(mag,unwrappedphase,outfile,nrow,ncol);
  }else if(params->outfileformat==ALT_SAMPLE_DATA){
    WriteAltSampFile(mag,unwrappedphase,outfile,nrow,ncol);
  }else if(params->outfileformat==FLOAT_DATA){
    Write2DArray((void **)unwrappedphase,outfile,nrow,ncol,sizeof(float));
  }else{
    fprintf(sp0,"WARNING: Illegal format specified for output file\n");
    fprintf(sp0,"         using default floating-point format\n");
    Write2DArray((void **)unwrappedphase,outfile,nrow,ncol,sizeof(float));
  }
}


/* function: OpenOutputFile()
 * --------------------------
 * Opens a file for writing.  If unable to open the file, tries to 
 * open a file in a dump path.
 */
FILE *OpenOutputFile(char *outfile){

  char outtok[MAXSTRLEN], *tempouttok, dumpfile[MAXSTRLEN];
  FILE *fp;

  if((fp=fopen(outfile,"w"))==NULL){

    /* if we can't write to the out file, get the file name from the path */
    /* and dump to the default path */
    StrNCopy(dumpfile,outfile,MAXSTRLEN);
    tempouttok=strtok(dumpfile,"/");
    StrNCopy(outtok,tempouttok,MAXSTRLEN);
    while((tempouttok=strtok(NULL,"/"))!=NULL){
      StrNCopy(outtok,tempouttok,MAXSTRLEN);
    }
    StrNCopy(dumpfile,DUMP_PATH,MAXSTRLEN);
    strcat(dumpfile,outtok);
    if((fp=fopen(dumpfile,"w"))!=NULL){
      fprintf(sp0,"WARNING: can't write to file %s.  Dumping to file %s\n",
	     outfile,dumpfile);
    }else{
      fprintf(sp0,"unable to write to file %s or dump to %s\nabort\n",
	     outfile,dumpfile);
      exit(ABNORMAL_EXIT);
    }
  }
  return(fp);

}


/* function: WriteAltLineFile()
 * ----------------------------
 * Writes magnitude and phase data from separate arrays to file.
 * Data type is float.  For each line of data, a full line of magnitude data
 * is written, then a full line of phase data.  Dumps the file to a 
 * default directory if the file name/path  passed in cannot be used.
 */
void WriteAltLineFile(float **mag, float **phase, char *outfile, 
		      long nrow, long ncol){

  int row;
  FILE *fp;

  fp=OpenOutputFile(outfile);
  for(row=0; row<nrow; row++){
    fwrite(mag[row],sizeof(float),ncol,fp);
    fwrite(phase[row],sizeof(float),ncol,fp);
  }
  fclose(fp);
}


/* function: WriteAltSampFile()
 * ----------------------------
 * Writes data from separate arrays to file, alternating samples.
 * Data type is float.  nrow and ncol are the sizes of each input
 * array.  Dumps the file to a default directory if the file name/path 
 * passed in cannot be used.
 */
void WriteAltSampFile(float **arr1, float **arr2, char *outfile, 
		      long nrow, long ncol){

  long row, col;
  FILE *fp;
  float *outline;

  outline=MAlloc(2*ncol*sizeof(float));
  fp=OpenOutputFile(outfile);
  for(row=0; row<nrow; row++){
    for(col=0;col<ncol;col++){
      outline[2*col]=arr1[row][col];
      outline[2*col+1]=arr2[row][col];
    }
    fwrite(outline,sizeof(float),2*ncol,fp);
  }
  fclose(fp);
}


/* function: Write2DArray()
 * ------------------------ 
 * Write data in a two dimensional array to a file.  Data elements are
 * have the number of bytes specified by size (use sizeof() when 
 * calling this function.  
 */
void Write2DArray(void **array, char *filename, long nrow, long ncol, 
		  size_t size){

  int row;
  FILE *fp;

  fp=OpenOutputFile(filename);
  for(row=0; row<nrow; row++){
    fwrite(array[row],size,ncol,fp);
  }
  fclose(fp);
}


/* function: Write2DRowColArray()
 * ------------------------------ 
 * Write data in a 2-D row-and-column array to a file.  Data elements 
 * have the number of bytes specified by size (use sizeof() when 
 * calling this function.  The format of the array is nrow-1 rows
 * of ncol elements, followed by nrow rows of ncol-1 elements each.
 */
void Write2DRowColArray(void **array, char *filename, long nrow, 
			long ncol, size_t size){

  int row;
  FILE *fp;

  fp=OpenOutputFile(filename);
  for(row=0; row<nrow-1; row++){
    fwrite(array[row],size,ncol,fp);
  }
  for(row=nrow-1; row<2*nrow-1; row++){
    fwrite(array[row],size,ncol-1,fp);
  }
  fclose(fp);
}


/* function: ReadInputFile()
 * -------------------------
 * Reads the input file specified on the command line.
 */
void ReadInputFile(char *infile, float ***magptr, float ***wrappedphaseptr, 
		   short ***flowsptr, long linelen, long nlines, 
		   paramT *params){

  long row, col, nrow, ncol;
  float **mag, **wrappedphase, **unwrappedphase;
  short **flows;

  /* initialize */
  mag=NULL;
  wrappedphase=NULL;
  unwrappedphase=NULL;
  flows=NULL;
  nrow=params->ntilerow;
  ncol=params->ntilecol;

  /* check data size */
  if(params->ntilecol>LARGESHORT || params->ntilerow>LARGESHORT){
    fprintf(sp0,"Input interferogram too large.  Abort\n");
    exit(ABNORMAL_EXIT);
  }
  if(params->ntilecol<3 || params->ntilerow<3){
    fprintf(sp0,"Input interferogram must be at least 3x3\n");
    exit(ABNORMAL_EXIT);
  }

  /* is the input file already unwrapped? */
  if(!params->unwrapped){

    /* read wrapped phase and possibly interferogram magnitude data */
    fprintf(sp1,"Reading wrapped phase from file %s\n",infile);
    if(params->infileformat==COMPLEX_DATA){
      ReadComplexFile(&mag,&wrappedphase,infile,linelen,nlines,params);
    }else if(params->infileformat==ALT_LINE_DATA){
      ReadAltLineFile(&mag,&wrappedphase,infile,linelen,nlines,params);
    }else if(params->infileformat==ALT_SAMPLE_DATA){
      ReadAltSampFile(&mag,&wrappedphase,infile,linelen,nlines,params);
    }else if(params->infileformat==FLOAT_DATA){
      ReadFloatFile(&wrappedphase,infile,linelen,nlines,params);
    }else{
      fprintf(sp0,"Illegal input file format specification.  Abort\n");
      exit(ABNORMAL_EXIT);
    }

    /* check to make sure the input data doesn't contain NaNs or infs */
    if(!ValidDataArray(wrappedphase,nrow,ncol) 
       || (mag!=NULL && !ValidDataArray(mag,nrow,ncol))){
      fprintf(sp0,"NaN or infinity found in input float data.  Abort\n");
      exit(ABNORMAL_EXIT);
    }

    /* flip the sign of the wrapped phase if baseline is positive */
    FlipPhaseArraySign(wrappedphase,params->bperp,nrow,ncol);

    /* make sure the wrapped phase is properly wrapped */
    WrapPhase(wrappedphase,nrow,ncol);

  }else{

    /* read unwrapped phase input */
    fprintf(sp1,"Reading unwrapped phase from file %s\n",infile);
    if(params->unwrappedinfileformat==ALT_LINE_DATA){
      ReadAltLineFile(&mag,&unwrappedphase,infile,linelen,nlines,params);
    }else if(params->unwrappedinfileformat==ALT_SAMPLE_DATA){
      ReadAltSampFile(&mag,&unwrappedphase,infile,
			   linelen,nlines,params);
    }else if(params->unwrappedinfileformat==FLOAT_DATA){
      ReadFloatFile(&unwrappedphase,infile,linelen,nlines,params);
    }else{
      fprintf(sp0,"Illegal input file format specification.  Abort\n");
      exit(ABNORMAL_EXIT);      
    }

    /* check to make sure the input data doesn't contain NaNs or infs */
    if(!ValidDataArray(unwrappedphase,nrow,ncol) 
       || (mag!=NULL && !ValidDataArray(mag,nrow,ncol))){
      fprintf(sp0,"NaN or infinity found in input float data.  Abort\n");
      exit(ABNORMAL_EXIT);
    }
    
    /* flip the sign of the input unwrapped phase if baseline is positive */
    FlipPhaseArraySign(unwrappedphase,params->bperp,nrow,ncol);

    /* parse flows of unwrapped phase */
    wrappedphase=ExtractFlow(unwrappedphase,&flows,nrow,ncol);

    /* free unwrapped phase array to save memory */
    Free2DArray((void **)unwrappedphase,nrow);

  }    

  /* get memory for mag (power) image and set to unity if not passed */
  if(mag==NULL){
    mag=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	mag[row][col]=1.0;
      }
    }
  }else{
    params->havemagnitude=TRUE;
  }
  
  /* set passed pointers and return the number of rows in data */
  *wrappedphaseptr=wrappedphase;
  *magptr=mag;
  *flowsptr=flows;

}


/* function: ReadMagnitude()
 * -------------------------
 * Reads the interferogram magnitude in the specfied file if it exists.
 * Memory for the magnitude array should already have been allocated by
 * ReadInputFile().
 */
void ReadMagnitude(float **mag, char *magfile, long linelen, long nlines, 
		   paramT *params){

  float **dummy;

  dummy=NULL;
  if(strlen(magfile)){
    fprintf(sp1,"Reading interferogram magnitude from file %s\n",magfile);
    if(params->magfileformat==FLOAT_DATA){
      ReadFloatFile(&mag,magfile,linelen,nlines,params);
    }else if(params->magfileformat==COMPLEX_DATA){
      ReadComplexFile(&mag,&dummy,magfile,linelen,nlines,params);
    }else if(params->magfileformat==ALT_LINE_DATA){
      ReadAltLineFile(&mag,&dummy,magfile,linelen,nlines,params);
    }else if(params->magfileformat==ALT_SAMPLE_DATA){
      ReadAltSampFile(&mag,&dummy,magfile,linelen,nlines,params);
    }
  }
  if(dummy!=NULL){
    Free2DArray((void **)dummy,params->ntilerow);
  }
  params->havemagnitude=TRUE;
}


/* function: ReadUnwrappedEstimateFile()
 * -------------------------------------
 * Reads the unwrapped-phase estimate from a file (assumes file name exists).
 *
 * 9-Feb-2001: This function hacked so that it can read unwrapped estimates
 * that are the same size as the tile to be read, not the size of the input
 * file.  Also modified paramT struct in snaphu.h and i/o routines 
 * SetDefaults(), ProcessArgs(), ReadConfigFile() so params->tilesizedest
 * can be set.
 */
void ReadUnwrappedEstimateFile(float ***unwrappedestptr, char *estfile, 
			       long linelen, long nlines, paramT *params){

  float **dummy;
  long nrow, ncol;
  paramT tempparams[1];


  /* initialize */
  dummy=NULL;
  memcpy(tempparams,params,sizeof(paramT));

  /* if unwrapped estimate is tile sized instead of full sized */
  if(tempparams->tilesizedest){
    linelen=tempparams->ntilecol;
    nlines=tempparams->ntilerow;
    tempparams->firstcol=1;
    tempparams->firstrow=1;
  }
  nrow=tempparams->ntilerow;
  ncol=tempparams->ntilecol;

  /* read data */
  fprintf(sp1,"Reading coarse unwrapped estimate from file %s\n",estfile);
  if(tempparams->estfileformat==ALT_LINE_DATA){
    ReadAltLineFilePhase(unwrappedestptr,estfile,linelen,nlines,tempparams);
  }else if(tempparams->estfileformat==FLOAT_DATA){
    ReadFloatFile(unwrappedestptr,estfile,linelen,nlines,tempparams);
  }else if(tempparams->estfileformat==ALT_SAMPLE_DATA){
    ReadAltSampFile(&dummy,unwrappedestptr,estfile,linelen,nlines,tempparams);
  }else{
    fprintf(sp0,"Illegal file format specification for file %s.  Abort\n",
	    estfile);
  }
  if(dummy!=NULL){
    Free2DArray((void **)dummy,nrow);
  }
  
  /* make sure data is valid */
  if(!ValidDataArray(*unwrappedestptr,nrow,ncol)){
    fprintf(sp0,"Infinity or NaN found in file %s.  Abort\n",estfile);
    exit(ABNORMAL_EXIT);
  }

  /* flip the sign of the unwrapped estimate if baseline is positive */
  FlipPhaseArraySign(*unwrappedestptr,tempparams->bperp,nrow,ncol);

}


/* function: ReadWeightsFile()
 * ---------------------------
 * Read in weights form rowcol format file of short ints.
 */
void ReadWeightsFile(short ***weightsptr,char *weightfile, 
			    long linelen, long nlines, paramT *params){

  long row, col, nrow, ncol;
  short **rowweight, **colweight;
  signed char printwarning;


  /* set up and read data */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(strlen(weightfile)){
    fprintf(sp1,"Reading weights from file %s\n",weightfile);
    Read2DRowColFile((void ***)weightsptr,weightfile,linelen,nlines,params
		     ,sizeof(short));
    rowweight=*weightsptr;
    colweight=&(*weightsptr)[nrow-1];
    printwarning=FALSE;
    for(row=0;row<nrow-1;row++){
      for(col=0;col<ncol;col++){
	if(rowweight[row][col]<0){
	  rowweight[row][col]=0;
	  printwarning=TRUE;
	}
      }
    }
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol-1;col++){
	if(colweight[row][col]<0){
	  colweight[row][col]=0;
	  printwarning=TRUE;
	}
      }
    }
    if(printwarning){
      fprintf(sp0,"WARNING: weights cannot be negative.  clipping to 0\n");
    }
  }else{
    fprintf(sp1,"No weight file specified.  Assuming uniform weights\n");
    *weightsptr=(short **)Get2DRowColMem(nrow,ncol,
					 sizeof(short *),sizeof(short));
    rowweight=*weightsptr;
    colweight=&(*weightsptr)[nrow-1];
    Set2DShortArray(rowweight,nrow-1,ncol,DEF_WEIGHT);
    Set2DShortArray(colweight,nrow,ncol-1,DEF_WEIGHT);
  }
}


/* function: ReadIntensity()
 * -------------------------
 * Reads the intensity information from specified file(s).  If possilbe,
 * sets arrays for average power and individual powers of single-pass
 * SAR images.  
 */
void ReadIntensity(float ***pwrptr, float ***pwr1ptr, float ***pwr2ptr, 
		   char *ampfile, char *ampfile2, long linelen, long nlines, 
		   paramT *params){
  
  float **pwr, **pwr1, **pwr2;
  long row, col, nrow, ncol;


  /* initialize */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  pwr=NULL;
  pwr1=NULL;
  pwr2=NULL;

  /* read the data */
  if(strlen(ampfile2)){

    /* data is given in two separate files */
    fprintf(sp1,"Reading brightness data from files %s and %s\n",
	    ampfile,ampfile2);
    if(params->ampfileformat==FLOAT_DATA){
      ReadFloatFile(&pwr1,ampfile,linelen,nlines,params);
      ReadFloatFile(&pwr2,ampfile2,linelen,nlines,params);
    }else{
      fprintf(sp0,"Illegal file formats specified for files %s, %s.  Abort\n",
	      ampfile,ampfile2);
      exit(ABNORMAL_EXIT);
    }

  }else{

    /* data is in single file */
    fprintf(sp1,"Reading brightness data from file %s\n",ampfile);
    if(params->ampfileformat==ALT_SAMPLE_DATA){
      ReadAltSampFile(&pwr1,&pwr2,ampfile,linelen,nlines,params);
    }else if(params->ampfileformat==ALT_LINE_DATA){
      ReadAltLineFile(&pwr1,&pwr2,ampfile,linelen,nlines,params);
    }else if(params->ampfileformat==FLOAT_DATA){
      ReadFloatFile(&pwr,ampfile,linelen,nlines,params);
      pwr1=NULL;
      pwr2=NULL;
    }else{
      fprintf(sp0,"Illegal file format specified for file %s.  Abort\n",
	      ampfile);
      exit(ABNORMAL_EXIT);
    }
  }

  /* check data validity */
  if((pwr1!=NULL && !ValidDataArray(pwr1,nrow,ncol)) 
     || (pwr2!=NULL && !ValidDataArray(pwr2,nrow,ncol))
     || (pwr!=NULL && !ValidDataArray(pwr,nrow,ncol))){
    fprintf(sp0,"Infinity or NaN found in amplitude or power data.  Abort\n");
    exit(ABNORMAL_EXIT);
  }

  /* if data is amplitude, square to get power */
  if(params->amplitude){
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	if(pwr1!=NULL && pwr2!=NULL){
	  pwr1[row][col]*=pwr1[row][col];
	  pwr2[row][col]*=pwr2[row][col];
	}else{
	  pwr[row][col]*=pwr[row][col];
	}
      }
    }
  }

  /* get the average power */
  if(pwr1!=NULL && pwr2!=NULL){
    if(pwr==NULL){
      pwr=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float));
    }
    for(row=0;row<nrow;row++){
      for(col=0;col<ncol;col++){
	pwr[row][col]=(pwr1[row][col]+pwr2[row][col])/2.0;
      }
    }
  }
  
  /* set output pointers */
  *pwrptr=pwr;
  *pwr1ptr=pwr1;
  *pwr2ptr=pwr2;

}


/* function: ReadCorrelation()
 * ---------------------------
 * Reads the correlation information from specified file.
 */
void ReadCorrelation(float ***corrptr, char *corrfile, 
		     long linelen, long nlines, paramT *params){
  
  float **corr, **dummy;
  long nrow;


  /* initialize */
  nrow=params->ntilerow;
  dummy=NULL;
  corr=NULL;

  /* read the data */
  fprintf(sp1,"Reading correlation data from file %s\n",corrfile);
  if(params->corrfileformat==ALT_SAMPLE_DATA){
    ReadAltSampFile(&dummy,&corr,corrfile,linelen,nlines,params);
  }else if(params->corrfileformat==ALT_LINE_DATA){
    ReadAltLineFilePhase(&corr,corrfile,linelen,nlines,params);
  }else if(params->corrfileformat==FLOAT_DATA){
    ReadFloatFile(&corr,corrfile,linelen,nlines,params);
  }else{
    fprintf(sp0,"Illegal file format specified for file %s.  Abort\n",
	    corrfile);
    exit(ABNORMAL_EXIT);
  }

  /* set output pointer and free memory */
  if(dummy!=NULL){
    Free2DArray((void **)dummy,nrow);
  }
  *corrptr=corr;

}


/* function: ReadAltLineFile()
 * ---------------------------
 * Read in the data from a file containing magnitude and phase
 * data.  File should have one line of magnitude data, one line
 * of phase data, another line of magnitude data, etc.  
 * ncol refers to the number of complex elements in one line of 
 * data.  
 */
void ReadAltLineFile(float ***mag, float ***phase, char *mpfile, 
		     long linelen, long nlines, paramT *params){

  FILE *fp;
  long filesize,row,nrow,ncol,padlen;

  /* open the file */
  if((fp=fopen(mpfile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",mpfile);
    exit(ABNORMAL_EXIT);
  }

  /* get number of lines based on file size and line length */ 
  fseek(fp,0,SEEK_END);            
  filesize=ftell(fp);
  if(filesize % (2*linelen*sizeof(float))){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    mpfile);
    exit(ABNORMAL_EXIT);
  }
  if(filesize/(2*linelen*sizeof(float))!=nlines){
    fprintf(sp0,"ERROR: file %s wrong size (%ldx%ld array expected)\n",
	    mpfile,nlines,linelen);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_SET);                 

  /* get memory */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*mag==NULL){
    if(((*mag)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }
  if(*phase==NULL){
    if(((*phase)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }
  
  /* read the data */
  fseek(fp,((params->firstrow-1)*2*linelen+(params->firstcol-1))
	*sizeof(float),SEEK_CUR);
  padlen=(linelen-ncol)*sizeof(float);
  for(row=0; row<nrow; row++){
    fread((*mag)[row],sizeof(float),ncol,fp);
    fseek(fp,padlen,SEEK_CUR);
    fread((*phase)[row],sizeof(float),ncol,fp);
    fseek(fp,padlen,SEEK_CUR);
  }
  fclose(fp);

}


/* function: ReadAltLineFilePhase()
 * --------------------------------
 * Read only the phase data from a file containing magnitude and phase
 * data.  File should have one line of magnitude data, one line
 * of phase data, another line of magnitude data, etc.  
 * ncol refers to the number of complex elements in one line of 
 * data. 
 */
void ReadAltLineFilePhase(float ***phase, char *mpfile, 
			  long linelen, long nlines, paramT *params){

  FILE *fp;
  long filesize,row,nrow,ncol,padlen;

  /* open the file */
  if((fp=fopen(mpfile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",mpfile);
    exit(ABNORMAL_EXIT);
  }

  /* get number of lines based on file size and line length */ 
  fseek(fp,0,SEEK_END);            
  filesize=ftell(fp);
  if(filesize % (2*linelen*sizeof(float))){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    mpfile);
    exit(ABNORMAL_EXIT);
  }
  if(filesize/(2*linelen*sizeof(float))!=nlines){
    fprintf(sp0,"ERROR: file %s wrong size (%ldx%ld array expected)\n",
	    mpfile,nlines,linelen);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_SET);                 

  /* get memory */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*phase==NULL){
    if(((*phase)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }
  
  /* read the phase data */
  fseek(fp,((params->firstrow-1)*2*linelen+linelen+(params->firstcol-1))
	*sizeof(float),SEEK_CUR);
  padlen=(2*linelen-ncol)*sizeof(float);
  for(row=0; row<nrow; row++){
    fread((*phase)[row],sizeof(float),ncol,fp);
    fseek(fp,padlen,SEEK_CUR);
  }
  fclose(fp);

}


/* function: ReadComplexFile()
 * ---------------------------
 * Reads file of complex floats of the form real,imag,real,imag...
 * ncol is the number of complex samples (half the number of real
 * floats per line).  Ensures that phase values are in the range 
 * [0,2pi).
 */
void ReadComplexFile(float ***mag, float ***phase, char *rifile, 
		     long linelen, long nlines, paramT *params){
         
  FILE *fp;
  long filesize,ncol,nrow,row,col,padlen;
  float *inpline;

  /* open the file */
  if((fp=fopen(rifile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",rifile);
    exit(ABNORMAL_EXIT);
  }

  /* get number of lines based on file size and line length */ 
  fseek(fp,0,SEEK_END);
  filesize=ftell(fp);
  if(filesize % (2*linelen*sizeof(float))){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    rifile);
    exit(ABNORMAL_EXIT);
  }
  if(filesize/(2*linelen*sizeof(float))!=nlines){
    fprintf(sp0,"ERROR: file %s wrong size (%ldx%ld array expected)\n",
	    rifile,nlines,linelen);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_SET);                 

  /* get memory */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*mag==NULL){
    if(((*mag)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }
  if(*phase==NULL){
    if(((*phase)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }  
  }
  if((inpline=(float *)malloc(2*ncol*sizeof(float)))==NULL){
    fprintf(sp0,"Out of Memory\n");
    exit(ABNORMAL_EXIT);
  }

  /* read the data and convert to magnitude and phase */
  fseek(fp,((params->firstrow-1)*linelen+(params->firstcol-1))
	*2*sizeof(float),SEEK_CUR);
  padlen=(linelen-ncol)*2*sizeof(float);
  for(row=0; row<nrow; row++){
    fread(inpline,sizeof(float),2*ncol,fp);
    for(col=0; col<ncol; col++){
      (*mag)[row][col]=sqrt(inpline[2*col]*inpline[2*col]
			    +inpline[2*col+1]*inpline[2*col+1]);
      if(!IsFinite((*phase)[row][col]=atan2(inpline[2*col+1],inpline[2*col]))){
	(*phase)[row][col]=0;
      }else if((*phase)[row][col]<0){
        (*phase)[row][col]+=TWOPI;
      }else if((*phase)[row][col]>=TWOPI){
        (*phase)[row][col]-=TWOPI;
      }
    }
    fseek(fp,padlen,SEEK_CUR);
  }
  free(inpline);
  fclose(fp);

}


/* function: ReadFloatFile()
 * -------------------------
 * Reads file of real floating-point data.  Assumes the native byte order 
 * of the platform. 
 */
void ReadFloatFile(float ***arr, char *infile,
		   long linelen, long nlines, paramT *params){
         
  FILE *fp;
  long filesize,row,nrow,ncol,padlen;

  /* open the file */
  if((fp=fopen(infile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",infile);
    exit(ABNORMAL_EXIT);
  }

  /* get number of lines based on file size and line length */ 
  fseek(fp,0,SEEK_END);
  filesize=ftell(fp);
  if(filesize % (linelen*sizeof(float))){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    infile);
    exit(ABNORMAL_EXIT);
  }
  if(filesize/(linelen*sizeof(float))!=nlines){
    fprintf(sp0,"ERROR: file %s wrong size (%ldx%ld array expected)\n",
	    infile,nlines,linelen);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_SET);                 

  /* get memory */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*arr==NULL){
    if(((*arr)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }

  /* read the data */
  fseek(fp,(linelen*(params->firstrow-1)+(params->firstcol-1))*sizeof(float),
	SEEK_CUR);
  padlen=(linelen-ncol)*sizeof(float);
  for(row=0; row<nrow; row++){
    fread((*arr)[row],sizeof(float),ncol,fp);
    fseek(fp,padlen,SEEK_CUR);
  }
  fclose(fp);

}


/* function: ReadAltSampFile()
 * ---------------------------
 * Reads file of real alternating floats from separate images.  Format is
 * real0A, real0B, real1A, real1B, real2A, real2B,...
 * ncol is the number of samples in each image (note the number of
 * floats per line in the specified file).
 */
void ReadAltSampFile(float ***arr1, float ***arr2, char *infile, 
		     long linelen, long nlines, paramT *params){
         
  FILE *fp;
  long filesize,row,col,nrow,ncol,padlen;
  float *inpline;

  /* open the file */
  if((fp=fopen(infile,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",infile);
    exit(ABNORMAL_EXIT);
  }

  /* get number of lines based on file size and line length */ 
  fseek(fp,0,SEEK_END);
  filesize=ftell(fp);
  if(filesize % (2*linelen*sizeof(float))){
    fprintf(sp0,"ERROR: extra data in file %s (bad linelength?)\n",
	    infile);
    exit(ABNORMAL_EXIT);
  }
  if(filesize/(2*linelen*sizeof(float))!=nlines){
    fprintf(sp0,"ERROR: file %s wrong size (%ldx%ld array expected)\n",
	    infile,nlines,linelen);
    exit(ABNORMAL_EXIT);
  }
  fseek(fp,0,SEEK_SET);                 

  /* get memory */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*arr1==NULL){
    if(((*arr1)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }
  }
  if(*arr2==NULL){
    if(((*arr2)=(float **)Get2DMem(nrow,ncol,sizeof(float *),sizeof(float)))
       ==NULL){
      fprintf(sp0,"Out of memory\n");
      exit(ABNORMAL_EXIT);
    }  
  }
  if((inpline=(float *)malloc(2*ncol*sizeof(float)))==NULL){
    fprintf(sp0,"Out of Memory\n");
    exit(ABNORMAL_EXIT);
  }

  /* read the data */
  fseek(fp,((params->firstrow-1)*linelen+(params->firstcol-1))
	*2*sizeof(float),SEEK_CUR);
  padlen=(linelen-ncol)*2*sizeof(float);
  for(row=0; row<nrow; row++){
    fread(inpline,sizeof(float),2*ncol,fp);
    for(col=0; col<ncol; col++){
      (*arr1)[row][col]=inpline[2*col];
      (*arr2)[row][col]=inpline[2*col+1];
    }
    fseek(fp,padlen,SEEK_CUR);
  }
  free(inpline);
  fclose(fp);

}


/* function: Read2DRowColFile()
 * ----------------------------
 * Gets memory and reads single array from a file.  Array should be in the 
 * file line by line starting with the row array (size nrow-1 x ncol) and
 * followed by the column array (size nrow x ncol-1).  Both arrays 
 * are placed into the passed array as they were in the file.
 */
void Read2DRowColFile(void ***arr, char *filename, long linelen, long nlines, 
		      paramT *params, size_t size){

  FILE *fp;
  long row, nel, nrow, ncol, padlen;
 
  /* open the file */
  if((fp=fopen(filename,"r"))==NULL){
    fprintf(sp0,"can't open file %s\nabort\n",filename);
    exit(ABNORMAL_EXIT);
  }

  /* get number of data elements in file */ 
  fseek(fp,0,SEEK_END);            
  nel=(long )(ftell(fp)/size);     
  fseek(fp,0,SEEK_SET);                 

  /* check file size */
  if(2*linelen*nlines-nlines-linelen != nel){
    fprintf(sp0,"ERROR: file %s wrong size\n",filename);
    exit(ABNORMAL_EXIT);
  }

  /* get memory if passed pointer is NULL */
  nrow=params->ntilerow;
  ncol=params->ntilecol;
  if(*arr==NULL){
    *arr=Get2DRowColMem(nrow,ncol,sizeof(void *),size);
  }

  /* read arrays */
  fseek(fp,(linelen*(params->firstrow-1)+(params->firstcol-1))*size,SEEK_SET);
  padlen=(linelen-ncol)*size;
  for(row=0; row<nrow-1; row++){
    fread((*arr)[row],size,ncol,fp);
    fseek(fp,padlen,SEEK_CUR);
  }
  fseek(fp,(linelen*(nlines-1)+(linelen-1)*(params->firstrow-1)
	    +(params->firstcol-1))*size,SEEK_SET);
  for(row=nrow-1; row<2*nrow-1; row++){
    fread((*arr)[row],size,ncol-1,fp);
    fseek(fp,padlen,SEEK_CUR);
  }
}


/* function: SetDumpAll()
 * ----------------------
 * Sets names of output files so that the program will dump intermediate
 * arrays.  Only sets names if they are not set already.
 */
void SetDumpAll(paramT *params){

  if(params->dumpall){
    if(params->initfile==NULL){
      params->initfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->initfile,DUMP_INITFILE,MAXSTRLEN);
    }
    if(params->flowfile==NULL){
      params->flowfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->flowfile,DUMP_FLOWFILE,MAXSTRLEN);
    }
    if(params->eifile==NULL){
      params->eifile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->eifile,DUMP_EIFILE,MAXSTRLEN);
    }
    if(params->rowcostfile==NULL){
      params->rowcostfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->rowcostfile,DUMP_ROWCOSTFILE,MAXSTRLEN);
    }
    if(params->colcostfile==NULL){
      params->colcostfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->colcostfile,DUMP_COLCOSTFILE,MAXSTRLEN);
    }
    if(params->mstrowcostfile==NULL){
      params->mstrowcostfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->mstrowcostfile,DUMP_MSTROWCOSTFILE,MAXSTRLEN);
    }
    if(params->mstcolcostfile==NULL){
      params->mstcolcostfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->mstcolcostfile,DUMP_MSTCOLCOSTFILE,MAXSTRLEN);
    }
    if(params->mstcostsfile==NULL){
      params->mstcostsfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->mstcostsfile,DUMP_MSTCOSTSFILE,MAXSTRLEN);
    }
    if(params->corrdumpfile==NULL){
      params->corrdumpfile=MAlloc(MAXSTRLEN*sizeof(char));
      StrNCopy(params->corrdumpfile,DUMP_CORRDUMPFILE,MAXSTRLEN);
    }
  }
}


/* function: SetStreamPointers()
 * -----------------------------
 * Sets the default stream pointers (global variables).
 */
void SetStreamPointers(void){

  fflush(NULL);
  if((sp0=DEF_ERRORSTREAM)==NULL){
    if((sp0=fopen(NULLFILE,"w"))==NULL){
      fprintf(sp0,"unable to open null file %s\n",NULLFILE);
      exit(ABNORMAL_EXIT);
    }
  }
  if((sp1=DEF_OUTPUTSTREAM)==NULL){
    if((sp1=fopen(NULLFILE,"w"))==NULL){
      fprintf(sp0,"unable to open null file %s\n",NULLFILE);
      exit(ABNORMAL_EXIT);
    }
  }
  if((sp2=DEF_VERBOSESTREAM)==NULL){
    if((sp2=fopen(NULLFILE,"w"))==NULL){
      fprintf(sp0,"unable to open null file %s\n",NULLFILE);
      exit(ABNORMAL_EXIT);
    }
  }
}


/* function: SetVerboseOut()
 * -------------------------
 * Set the global stream pointer sp2 to be stdout if the verbose flag
 * is set in the parameter data type.
 */
void SetVerboseOut(paramT *params){

  fflush(NULL);
  if(params->verbose){
    if(sp2!=stdout && sp2!=stderr && sp2!=stdin && sp2!=NULL){
      fclose(sp2);
    }
    sp2=stdout;
  }
}


/* function: DumpIncrCostFiles()
 * -----------------------------
 * Dumps incremental cost arrays, creating file names for them.
 */
void DumpIncrCostFiles(short ***incrcosts, long iincrcostfile, 
		       long nflow, long nrow, long ncol){

  char incrcostfile[MAXSTRLEN];
  char tempstr[MAXSTRLEN];

  /* create the file names and dump the files */
  /* snprintf() is more elegant, but its unavailable on some machines */
  strncpy(incrcostfile,INCRCOSTFILEPOS,MAXSTRLEN-1);
  sprintf(tempstr,".%ld_%ld",iincrcostfile,nflow);
  strncat(incrcostfile,tempstr,MAXSTRLEN-strlen(incrcostfile)-1);
  Write2DRowColArray((void **)incrcosts[POSINCR],incrcostfile,
		     nrow,ncol,sizeof(short));
  strncpy(incrcostfile,INCRCOSTFILENEG,MAXSTRLEN-1);
  sprintf(tempstr,".%ld_%ld",iincrcostfile,nflow);
  strncat(incrcostfile,tempstr,MAXSTRLEN-strlen(incrcostfile)-1);
  Write2DRowColArray((void **)incrcosts[NEGINCR],incrcostfile,
		     nrow,ncol,sizeof(short));

}


