/* SccsId[]= @(#)util_save.c	2.41 3/24/98 */
#include <stdio.h>
#include "error.h"
static char sccsid_util_save[]= "@(#)SSP2util_save.c:2.41";                    

main(argc,argv)
int argc;
char *argv[];
{
int istatus;
int numtask,taskid,rc;
char buff[500];
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
 
 
 rc = 0;

 if (rc) 
  {
   istatus=ierr_1;
  }
 else
  {
   if (taskid==0)
     {
      strcpy(buff,"cp ");
      strcat(buff,fileodl);
      strcat(buff," ");
      strcat(buff,save_dir_input);
      strcat(buff,"FAILED.");
      strcat(buff,argv[2]);
      strcat(buff," 2>> ");
      strcat(buff,filesyserror);
      icode=system(buff);
      icode=icode/256;
     }
/****************************/
   strcpy(buff,"cp ");
   strcat(buff,"/home/tmpdisk/");
   strcat(buff,"*.");
   strcat(buff,argv[2]);
   strcat(buff," ");
   strcat(buff,save_dir_data);
   strcat(buff," 2>> ");
   strcat(buff,filesyserror);
   icode=system(buff);
   icode=icode/256;
/****************************/
  }
 MPI_Finalize();
 exit(istatus);
}
