/* SccsId[]= @(#)util_copy.c	2.41 3/24/98 */
#include <stdio.h>
#include "error.h"
static char sccsid_util_copy[]= "@(#)SSP2util_copy.c:2.41";                    

main(argc,argv)
int argc;
char *argv[];
{
int istatus;
int numtask,taskid,rc;
char FILE_COPYBACK[256],buff[500];
FILE *fp;
int icode;
int proc_id;
char save_dir_data[256];
char save_dir_input[256];
char sourcedir[256];
int ire_sim;
char fileodl[256],filestatus[256];
int burst_start,burst_end,sim_eam_nsp[4];
char filesyserror[256];
char filetime[256],perf_flag[10];
char time_file[256],dump_file[256];
int num_node;
char cmd[500];
int ant_flag;

 
rc= MPI_Init(&argc,&argv);
if (rc) exit(ierr_1);
rc= MPI_Comm_size(MPI_COMM_WORLD,&numtask);
if (rc) exit(ierr_1);
rc= MPI_Comm_rank(MPI_COMM_WORLD,&taskid);
if (rc) exit(ierr_1);

 istatus=iok;

 c_load_config(argv[1],&num_node,perf_flag,save_dir_data,save_dir_input,sourcedir,&ire_sim,fileodl,filestatus,filetime,filesyserror,&burst_start,&burst_end,sim_eam_nsp,time_file,dump_file,&ant_flag);


 sscanf(argv[2],"%d",&proc_id);
 build_script_name("/home/tmpdisk/",proc_id,"COPY",FILE_COPYBACK);
 fp=fopen(FILE_COPYBACK,"r");

 rc = 0;
 if (rc) 
  {
   istatus=ierr_1;
  }
 else
  {
   if (taskid==0)
    {
     while (fgets(buff,500,fp)!=NULL)
      {
        rm_newline(buff);
        strcpy(cmd,buff);
        strcat(cmd," 2>> ");
        strcat(cmd,filesyserror);
        icode=retry_submit(cmd);
        if (icode!=0)        
         {
          istatus=ierr_13;
          exit(istatus);
         }
      }
    }
  }
 MPI_Finalize();
 exit(istatus);
}

int rm_newline(buff)
char* buff;
{
 int i;
 for (i=strlen(buff)-1;i>=0;i--)
  {
    if (buff[i]=='\n')
     {
      buff[i]=' ';
      return(1); 
     }
  return(1); 
  }


}
int retry_submit(cmd)
char *cmd;
{
int icode,i;

 for (i=0;i<3;i++)
  {
   icode=system(cmd);
   icode=icode/256;
   if (icode==0) break;
  }

 return(icode);



}
