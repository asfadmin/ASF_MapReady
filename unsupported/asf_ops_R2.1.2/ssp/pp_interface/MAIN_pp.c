/* SccsId[]= @(#)MAIN_pp.c	2.41 3/24/98 */
static char sccsid_MAIN_pp[]= "@(#)PPMAIN_pp.c:2.41";


#include <stdio.h>
#include <stdlib.h>
#include "asf.h"
#include <fcntl.h>
#include "error.h"
#include "ssp_req_odl.h"


static char FILE_ENG_CONFIG[256];
static char FILE_STATUS[256];
static char FILE_PATH[256];
static char FILE_ODL[256];
static char PROCESSOR_SAVEDIR[256];
static char CONTROL_SAVEDIR[256];
static char FILE_TIME[256];
static char FILE_SYSERROR[256];
static char PERF_FLAG[10];
static char subsystem_name[256];
static int   IRE_SIM;
static int  job_id;
static int  job_nbeam;
static int  job_pixel;
static char job_projection[256];
static int NUM_NODE;
static int RESET=0;
static char time_file[256],dump_file[256];

/*******************************************************************************/

static int SendAckToServer (char* ack_type, int errno, char* text, ODL msg, int itype);
static void  ssp_scan (ODL msg, void *arg);
static void ssp_heartbeat(ODL msg, void *arg); 
static void ssp_cleanup(ODL msg, void *arg);
static void ssp_reset(ODL msg, void *arg);

/*******************************************************************************/

int main(int argc, char *argv[])
{
    AsfApp app;
    int i;
    FILE *fp;
    int istatus;
    int dummy1,dummy2,dummy3[4];
    char buff_temp[300];
    int ant_flag;

    strcpy(FILE_ENG_CONFIG, NULL);

    for (i=1;i<argc;i++)
     {
       if (strcmp(argv[i],"-configfile")==0)
        {
         strcpy(FILE_ENG_CONFIG,argv[i+1]);
        }
     }

    if (FILE_ENG_CONFIG == NULL) {
       dump_error_and_exit("NOT VALID SSP2/PP CONFIGURATION FILE",1);
    }


    printf("Hello\n");
    fflush(stdout);

    c_load_config(FILE_ENG_CONFIG,&NUM_NODE,PERF_FLAG,PROCESSOR_SAVEDIR,CONTROL_SAVEDIR,FILE_PATH,&IRE_SIM,FILE_ODL,FILE_STATUS,FILE_TIME,FILE_SYSERROR,&dummy1,&dummy2,dummy3,time_file,dump_file,&ant_flag);
    printf("FILE_ENG_CONFIG  = %s\n",FILE_ENG_CONFIG);
    printf("FILE_PATH  = %s\n",FILE_PATH);
    printf("NUM OF NODES  = %d\n",NUM_NODE);
    printf("FILE_ODL   = %s\n",FILE_ODL);
    printf("FILE_TIME   = %s\n",FILE_TIME);
    printf("FILE_STATUS= %s\n",FILE_STATUS);
    printf("FILE_SYSERROR= %s\n",FILE_SYSERROR);
    printf("PROCESSOR_SAVEDIR= %s\n",PROCESSOR_SAVEDIR);
    printf("CONTROL_SAVEDIR= %s\n",CONTROL_SAVEDIR);
    printf("IRE_SIM    = %d\n",IRE_SIM);
    fflush(stdout);

  strcpy(subsystem_name,"SSP2");
  for (i = 1; i < argc; i++)
   {
        if (strcmp(argv[i], "-asfn") == 0)
          {
            if ((i + 1) < argc)
             {
                strcpy(subsystem_name,argv[i+1]);
                break;
              }
          }
   }


   printf("SUBSYSTEM NAME %s\n",subsystem_name);
   fflush(stdout);

    app = AsfAppInitialize (subsystem_name, &argc, argv);
    if (app == NULL) {
       dump_error_and_exit("can't initialize SSP application.",1);
    }

      AsfAddCallback(app, 
                          "SUBSYSTEM_HEARTBEAT", ssp_heartbeat,0 ,
                          "SUBSYSTEM_HALT"     , ssp_reset    ,0 , 
                          "SPS_FRAME_REQUEST"  , ssp_scan     ,0 ,
                          "SUBSYSTEM_STOP"     , ssp_reset    ,0 ,
                          "CLEANUP_REQUEST"    , ssp_cleanup    ,0 ,
                          NULL);

    istatus=init_status(FILE_STATUS);
    if ((istatus>=ierr_lower) && (istatus<=ierr_upper))
     {
       sprintf(buff_temp,"%s %s\n",ssp2_msg[istatus],FILE_STATUS);
       dump_error_and_exit(buff_temp,1);
     }

    AsfAppMainLoop(app);
}

/*****************************************************************************/

static int SendAckToServer (char* ack_type, int errno, char* text, ODL msg, int itype)
{
    ODL ack;
    char MESSAGE[100],STATUS[100],SUB_TYPE[100];
    char errno_str[20];
 

    if (! msg) {
        dump_error_and_exit("PROBLEMS SENDING MESSAGE TO CP",1);
    }
    if (! (ack = GetAckMsg(msg, ack_type))) {
        dump_error_and_exit("PROBLEMS SENDING MESSAGE TO CP",1);
    }
 
    switch (errno)
    {
	case 0  : 
             strcpy(errno_str, "COMPLETED");
             break;
	case -1 :
	     strcpy(errno_str, "CANCEL/FAIL");
             break;
        case -2 :
             strcpy(errno_str, "WRONG_TAPE");
             break;
        default :
             strcpy(errno_str, "UNKNOWN");
    }  /* end of switch */

    if (strcmp(ack_type, "SUBSYSTEM_STATUS")==0) 
      {
         sprintf(MESSAGE ,"%s","SUBSYSTEM_STATUS.BODY.COMMENT");
         sprintf(STATUS  ,"%s","SUBSYSTEM_STATUS.BODY.STATUS");
         sprintf(SUB_TYPE,"%s","SUBSYSTEM_STATUS.BODY.SUB_TYPE");
      ODLSetVal(ack, STATUS, errno_str);
      ODLSetVal(ack, MESSAGE, text);
      }
    if (strcmp(ack_type, "SUBSYSTEM_COMPLETED")==0) 
      {
         sprintf(MESSAGE ,"%s","SUBSYSTEM_COMPLETED.BODY.COMMENT");
         sprintf(STATUS  ,"%s","SUBSYSTEM_COMPLETED.BODY.STATUS");
         sprintf(SUB_TYPE,"%s","SUBSYSTEM_COMPLETED.BODY.SUB_TYPE");
      ODLSetVal(ack, STATUS, errno_str);
      ODLSetVal(ack, MESSAGE, text);
      }
    if (strcmp(ack_type, "SUBSYSTEM_ACK")==0) 
      {
      }
    if (strcmp(ack_type, "SUBSYSTEM_STATUS")==0) {
      if(itype==1)
       {
        ODLSetVal(ack, SUB_TYPE, "HEARTBEAT");
        printf("==============> RESPONDING HEARTBEAT FROM CP\n");
        fflush(stdout);
       }
      if(itype==2)
       {
        ODLSetVal(ack, SUB_TYPE, "CLEANUP");
        printf("==============> RESPONDING CLEANUP FROM CP\n");
        fflush(stdout);
       }
    }
    WriteMsgToServer(ack);
    ODLFree(ack);
    return 0;
}


/*****************************************************************************/

static void  ssp_scan (ODL msg, void *arg)
{
 char f_odl[256],f_status[256],str_return[300];
 int istatus;
 char id_char[10];
 char cmd[500];
 int icode;
 char name[300];

 SendAckToServer("SUBSYSTEM_ACK", 0, "Greetings from SSP2!", msg, 0);
/*run the code*/
 if (IRE_SIM==1)
  {
  istatus=dump_odl_to_disk(FILE_ODL,msg);                               
  if ((istatus>=ierr_lower) && (istatus<=ierr_upper))                   
     {                                                                  
       strcpy(str_return, ssp2_msg[istatus]);                           
       readerr(istatus,name);
       strcat(str_return,name);
       istatus= -1;                                                     
       SendAckToServer("SUBSYSTEM_COMPLETED", istatus, str_return, msg, 0);
       dump_error_and_exit("DUMPING SPS_FRAME_REQUEST FAILED",0);
       return;                                                          
     }                                                                  
     
   } 
   istatus=get_jobid(FILE_ODL,&job_id,&job_nbeam,&job_pixel,job_projection);
   if ((istatus>=ierr_lower) && (istatus<=ierr_upper))
       {
         strcpy(str_return, ssp2_msg[istatus]); 
         readerr(istatus,name);
         strcat(str_return,name);
         istatus= -1;                                                    
         SendAckToServer("SUBSYSTEM_COMPLETED", istatus, str_return, msg, 0);
         dump_error_and_exit("PARSING THROUGH SPS_FRAME_REQUEST FAILED",0);
         return;                                       
       }
   printf("job_id is %d\n",job_id);
   fflush(stdout);
   printf("job_nbeam is %d\n",job_nbeam);
   fflush(stdout);

   if ((job_nbeam == 2) || (job_nbeam == 3) || (job_nbeam == 4))
     istatus=run_the_code(FILE_PATH,FILE_ENG_CONFIG,job_id,subsystem_name,time_file);
   else
     istatus=run_pp(FILE_PATH, FILE_ENG_CONFIG, job_id, subsystem_name, time_file); 

 if (RESET==1)
  {
   RESET=0;
   return;
  }
 if (istatus!=0)
  {
/* if (istatus<0)  return;    */
   if (istatus>0) 
    {
     strcpy(str_return, ssp2_msg[istatus]);
     readerr(istatus,name);
     strcat(str_return,name);
     istatus= -1;
     SendAckToServer("SUBSYSTEM_COMPLETED", istatus, str_return, msg, 0);
    }
  }
 else
  {/*SUCCESS*/
    strcpy(str_return,ssp2_msg[istatus]);
    readerr(istatus,name);
    strcat(str_return,name);
    sprintf(id_char,"%d",job_id);
    strcpy(cmd,FILE_PATH);
    strcat(cmd,"/run.util_copy ");
    strcat(cmd,FILE_ENG_CONFIG);
    strcat(cmd," ");
    strcat(cmd,id_char);
    icode=system(cmd); 
    icode=icode/256;
    if (icode !=0)
     {
      strcpy(str_return,ssp2_msg[icode]);
      readerr(icode,name);
      strcat(str_return,name);
      istatus= -1;
      dump_error_and_exit("REMOTE COPY AFTER PROCESSING FAILED",0);
     }
    if (istatus == 0)
     {
      strcpy(cmd,FILE_PATH);
      strcat(cmd,"/run.util_cleanup ");
      strcat(cmd,FILE_ENG_CONFIG);
      strcat(cmd," ");
      strcat(cmd,id_char);
      icode=system(cmd); 
      icode=icode/256;
      if (icode !=0)
       {
        strcpy(str_return,ssp2_msg[icode]);
        readerr(icode,name);
        strcat(str_return,name);
        istatus= -1;
        dump_error_and_exit("CLEANUP AFTER PROCESSING FAILED",0);
       }
     }
    SendAckToServer("SUBSYSTEM_COMPLETED", istatus, str_return, msg, 0);

  }
/*run the code*/
 init_status(FILE_STATUS);

 return;
}

/*****************************************************************************/

static void ssp_heartbeat(ODL msg, void *arg) 
{
char wayne_state_str[60];
float time_mark[10];
int istage;
float ipercent;
int istatus;
char str_return[300];
float calculate_percent(),report_percent;

printf("CHECKING*******************\n");
fflush(stdout);
istatus= lookup_status(FILE_STATUS,&istage,&ipercent);
if ((istatus>=ierr_lower) && (istatus<=ierr_upper))
  {
       /*
         strcpy(str_return, ssp2_msg[istatus]);
         istatus= -1;
         SendAckToServer("SUBSYSTEM_STATUS"   , istatus, str_return, msg, 1);
        */
         sprintf(wayne_state_str,"STATUS is not available");
         SendAckToServer("SUBSYSTEM_STATUS", 0, wayne_state_str, msg, 1);
         return;
  }
if(ipercent >= 0 ) {
  istatus= lookup_time  (FILE_TIME,time_mark,job_nbeam,job_pixel,job_projection);
  if ((istatus>=ierr_lower) && (istatus<=ierr_upper))
    {
       /*
         strcpy(str_return, ssp2_msg[istatus]);
         istatus= -1;
         SendAckToServer("SUBSYSTEM_STATUS"   , istatus, str_return, msg, 1);
       */
         sprintf(wayne_state_str,"STATUS is not available");
         SendAckToServer("SUBSYSTEM_STATUS", 0, wayne_state_str, msg, 1);
         return;
    }
  report_percent=calculate_percent(time_mark,istage,ipercent,NUM_NODE);
    if (istage==0)
      sprintf(wayne_state_str,"%s","PP/SSP IDLING");
    if (istage==1)
      sprintf(wayne_state_str,"SSP DATA TRANSFER: %3.1f %% completion",ipercent*100.0);
    if (istage==2)
      sprintf(wayne_state_str,"SSP PROCESSING: %3.1f %% completion",report_percent);
    if (istage==3)
      sprintf(wayne_state_str,"SSP PROCESSING: %3.1f %% completion",report_percent);
    if (istage==4)
      sprintf(wayne_state_str,"SSP PROCESSING: %3.1f %% completion",report_percent);
    if (istage==5)
      sprintf(wayne_state_str,"SSP PROCESSING: %3.1f %% completion",report_percent);
    SendAckToServer("SUBSYSTEM_STATUS", 0, wayne_state_str, msg, 1);
} else {
    if(istage==0) 
      sprintf(wayne_state_str,"%s","PP/SSP IDLING");
    if(istage==1)
      sprintf(wayne_state_str,"PP DATA TRANSFER: %3.0f %% completion",-ipercent*100.0);
    if(istage==3)
      sprintf(wayne_state_str,"PP PROCESSING: %3.0f %% completion",-ipercent*100.0);
    if(istage==5)
      sprintf(wayne_state_str,"PP GENERATE CEOS: %3.0f %% completion",-ipercent*100.0);
    if(istage==6)
      sprintf(wayne_state_str,"PP TRANSFER IMAGE BACK: %3.0f %% completion",-ipercent*100.0);
    SendAckToServer("SUBSYSTEM_STATUS", 0, wayne_state_str, msg, 1);
}
return;
}

/*****************************************************************************/

static void ssp_cleanup(ODL msg, void *arg)
{
 char save_flag[60];
 char save_dir[60];
 int ierr;
 char *error_msg;

/*  strcpy(save_flag, ODLGetString(msg, "CLEANUP_REQUEST.BODY.SAVE_FLAG", &ierr));*/
    strcpy(save_flag, ODLGetStr(msg, "CLEANUP_REQUEST.BODY.SAVE_FLAG"));
  if (save_flag==NULL) {
    dump_error_and_exit("PROBLEMS PARSING THROUGH CLEANUP REQUEST",1);
  }
 cleanup(save_flag,PROCESSOR_SAVEDIR,CONTROL_SAVEDIR,job_id);
 SendAckToServer("SUBSYSTEM_STATUS", 0, "CLEAN UP COMPLETED", msg, 2);
 return;

}

/*****************************************************************************/

static void ssp_reset(ODL msg, void *arg)
{
 int icode;
 char cmd[500];
 char id_char[10];

 strcpy(cmd,FILE_PATH);
 strcat(cmd,"/kill.script");
 icode=system(cmd); 
 icode=icode/256;
 RESET=icode;
 strcpy(cmd,FILE_PATH);
 strcat(cmd,"/run.util_cleanup ");
 strcat(cmd,FILE_ENG_CONFIG);
 strcat(cmd," ");
 sprintf(id_char,"%d",job_id);
 strcat(cmd,id_char);
 icode=system(cmd); 
 icode=icode/256;
 init_status(FILE_STATUS);
 return;
}

/*****************************************************************************/

int init_status(file_status)
char *file_status;
{
 int istat;
 FILE *dummy_fp;
 char buff[300];
 
 if ((dummy_fp=fopen(file_status,"w"))== NULL)
    {
     printf("cannot open %s\n",file_status);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_status);
     printclog(1,buff);
     printerr(file_status);
     return(ierr_2);
    }
 fprintf(dummy_fp,"0  0.0\n");
 fflush(dummy_fp);
 fclose(dummy_fp);

    
 return(0);
 
}

/*****************************************************************************/

int dump_error_and_exit(str,iseverity)
char *str;
int iseverity;
{
 

   engage_log(subsystem_name);
   printclog(1,str);
   printf("%s\n",str);
   fflush(stdout);
   disengage_log();

   if (iseverity==1)
     {
      exit(1);
     }
   return(1);

}

/*****************************************************************************/

int set_status(file_status,istage,ipercent)
int istage;
float ipercent;
char *file_status;
{
 FILE *dummy_fp;
 char buff[300];
 if ((dummy_fp=fopen(file_status,"a"))== NULL)
    {
     printf("cannot open %s\n",file_status);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_status);
     printclog(1,buff);
     printerr(file_status);
     return(ierr_2);
    }
 fprintf(dummy_fp,"%d %f\n",istage,ipercent);
 fflush(dummy_fp);
 fclose(dummy_fp);
 return(0);
}
/*****************************************************************************/
int lookup_status(file_status,istage,ipercent)
int *istage;
float *ipercent;
char* file_status;
{
 FILE* dummy_fp;
 char buff[300];

 if ((dummy_fp=fopen(file_status,"r"))== NULL)
    {
     printf("cannot open %s\n",file_status);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_status);
     printclog(1,buff);
     printerr(file_status);
     return(ierr_2);
    }
 while (fscanf(dummy_fp,"%d%f",istage,ipercent)!=EOF);
 fclose(dummy_fp);
 if (*ipercent >= 0)
   *ipercent= *ipercent/100.0;

 
 return(0);
}

/*****************************************************************************/

int lookup_time(file_time,a,nbeam,pixel,projection)
float *a;
char* file_time;
int nbeam;
int pixel;
char *projection;
{
 FILE* dummy_fp;
 char buff[300];
 int found;
 int d_nbeam,d_pixel;
 char d_projection[60];


 if ((dummy_fp=fopen(file_time,"r"))== NULL)
    {
     printf("cannot open %s\n",file_time);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",file_time);
     printclog(1,buff);
     printerr(file_time);
     return(ierr_2);
    }

 found=0;
 while (found==0)
  {
   fgets(buff,300,dummy_fp);
   sscanf(buff,"%d%d%s",&d_nbeam,&d_pixel,d_projection);
   if ((d_nbeam==nbeam)&&(d_pixel==pixel)&&(strcmp(projection,d_projection)==0))
    {
     sscanf(buff,"%d%d%s%f%f%f%f%f",&d_nbeam,&d_pixel,d_projection,&a[1],&a[2],&a[3],&a[4],&a[5]);
     found=1;
    }
  }


  fclose(dummy_fp);

 return(0);

}

/*****************************************************************************/

float calculate_percent(time_mark,istage,ipercent,num_node)

float *time_mark;
int istage;
float ipercent;
int num_node;
{
 float total_time;
 float a_completion;
 int i;

 printf("TIME INFO : %f %f %f %f %f\n",time_mark[1],time_mark[2],time_mark[3],time_mark[4],time_mark[5]);
 fflush(stdout);

 if ((istage==0)||(istage==1))
  {
   return(0.0);
  }
 else
  {
   total_time=time_mark[2]+time_mark[3]*4/num_node+time_mark[4]+time_mark[5];
   a_completion=0.0;
   if (istage==2)
    {
     a_completion=time_mark[istage]*ipercent+a_completion;
    }
   else
   {
     for (i=2;i<istage;i++)
      a_completion=time_mark[i]+a_completion;
     a_completion=time_mark[istage]*ipercent+a_completion;
   }
   a_completion =a_completion/total_time*100.0;
   return(a_completion);
  }
}

/*****************************************************************************/

int get_jobid(filename,proc_id,i_instr,i_pixel,projection)
char* filename;
int *proc_id;
int *i_instr,*i_pixel;
char *projection;
{
    int ierr;
    ODL odl;
    struct stat stbuf;
    char *buf;
    char *sss;
    int fd;
    char *error_msg;
/****************************************/
    FILE *dummy_fp;
    double pixel_spacing;
    char  instr_mode[60];
    FILE   *TIME_FILE_FP;
    char buff[300];



    if ((fd = open(filename, 0)) == -1) {
      sprintf(buff,"cannot open %s during parsing %s",filename,filename);
      printclog(1,buff);
      printerr(filename);
      return (ierr_2);
    }
    if (fstat(fd, &stbuf) == -1) {
      sprintf(buff,"cannot open %s during parsing %s",filename,filename);
      printclog(1,buff);
      printerr(filename);
        return (ierr_2);
    }

    if ((buf = (char*) malloc(stbuf.st_size)) == NULL) {
        sprintf(buff,"cannot malloc during parsing %s",filename);
        printclog(1,buff);
        return (ierr_5);
    }
    if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
        sprintf(buff,"cannot read during parsing %s",filename);
        printclog(1,buff);
        printerr(filename);
        return (ierr_4);
    }
    close (fd);


    if ((odl = StrToODL(buf, stbuf.st_size)) == NULL) {
        sprintf(buff,"cannot parse through %s",filename);
        printclog(1,buff);
        return (ierr_11);
    }

  free(buf);


  *proc_id = ODLGetInt(odl, JOB_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get JOB_ID");
    sprintf(buff,"%s",error_msg);
    printclog(1,buff);
    return(ierr_12);
  }

/*  strcpy(projection , ODLGetString(odl, PROJECTION, &ierr));*/
    strcpy(projection , ODLGetStr(odl, PROJECTION));  
  if (projection == NULL) {
    strcpy(error_msg , "Can't get PROJECTION");
    sprintf(buff,"%s",error_msg);
    printclog(1,buff);
    return(ierr_12);
  }
  pixel_spacing = ODLGetDouble(odl, PIXEL_SPACING, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "Can't get PIXEL_SPACING");
    sprintf(buff,"%s",error_msg);
    printclog(1,buff);
    return(ierr_12);
  }
  *i_pixel=pixel_spacing;
/*  strcpy(instr_mode , ODLGetString(odl, INSTRUMENT_MODE, &ierr));*/
    strcpy(instr_mode , ODLGetStr(odl, INSTRUMENT_MODE));
  if (instr_mode==NULL) {
    strcpy(error_msg , "Can't get MODE");
    sprintf(buff,"%s",error_msg);
    printclog(1,buff);
    return(ierr_12);
  }
  if (strcmp(instr_mode,"SWA")==0) *i_instr= 4;
  else if (strcmp(instr_mode,"SWB")==0) *i_instr= 4;
  else if (strcmp(instr_mode,"SNA")==0) *i_instr= 2;
  else if (strcmp(instr_mode,"SNB")==0) *i_instr= 3;
  else *i_instr = 0;

 if ((TIME_FILE_FP=fopen(time_file,"a"))== NULL)
    {
     printf("cannot open TIME_FILE\n");
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",time_file);
     printclog(1,buff);
     return(ierr_2);
    }
 fprintf(TIME_FILE_FP,"***********%d %d %s\n",*i_instr,*i_pixel,projection);
 fclose(TIME_FILE_FP);
 

 return(0);

}

/*******************************************************************/

int cleanup(save_flag,save_dir_data,save_dir_input,id)
int id;
char *save_flag;
char *save_dir_data;
char *save_dir_input;
{
char cmd[500];
int icode;
char id_char[10];

sprintf(id_char,"%d",id);

 if (strcmp(save_flag,"YES")==0)
  {
   strcpy(cmd,FILE_PATH);
   strcat(cmd,"/run.util_save ");
   strcat(cmd,FILE_ENG_CONFIG);
   strcat(cmd," ");
   strcat(cmd,id_char);
   icode=system(cmd); 
   icode=icode/256;

  }
 strcpy(cmd,FILE_PATH);
 strcat(cmd,"/run.util_cleanup ");
 strcat(cmd,FILE_ENG_CONFIG);
 strcat(cmd," ");
 strcat(cmd,id_char);
 icode=system(cmd); 
 icode=icode/256;


 return;
}
