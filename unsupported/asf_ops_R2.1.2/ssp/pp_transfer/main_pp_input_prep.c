/* SccsId[]= @(#)main_pp_input_prep.c	2.41 3/24/98 */
static char sccsid_main_pp_input_prep[]= "@(#)PPmain_pp_input_prep.c:2.41";

#include <stdio.h>
#include "error.h"
#include "mpi.h"

main(argc,argv)
int argc;
char *argv[];
{
char file_config[256];
char file_config_f[256];
int istatus;
int numtask,taskid,rc;
int i_file_type;


rc= MPI_Init(&argc,&argv);
if (rc) exit(ierr_1);
rc= MPI_Comm_size(MPI_COMM_WORLD,&numtask);
if (rc) exit(ierr_1);
rc= MPI_Comm_rank(MPI_COMM_WORLD,&taskid);
if (rc) exit(ierr_1);
 istatus=iok;
 strcpy(file_config,argv[1]);
 i_file_type=atoi(argv[2]);

   engage_log(argv[3]);
   str_c_to_f(file_config_f,file_config,256);

   pp_input_prep(&i_file_type,file_config_f,&istatus,&numtask,&taskid);
   printf("main:: istatus = %d\n", istatus);
   disengage_log();
   
   if (istatus != 0)
	MPI_Abort(MPI_COMM_WORLD, istatus);


 MPI_Finalize();

 exit(istatus); 
}
