/* SccsId[]= @(#)main_pre_processor.c	2.41 3/24/98 */
#include <stdio.h>
#include "error.h"
static char sccsid_preprocessor[]=  "@(#)SSP2main_pre_processor.c:2.41";                    

main(argc,argv)
int argc;
char *argv[];
{
char file_config[256];
char file_config_f[256];
int  istatus;
int numtask,taskid,rc;


rc= MPI_Init(&argc,&argv);
if (rc) exit(ierr_1);
rc= MPI_Comm_size(MPI_COMM_WORLD,&numtask);
if (rc) exit(ierr_1);
rc= MPI_Comm_rank(MPI_COMM_WORLD,&taskid);
if (rc) exit(ierr_1);
istatus=iok;
strcpy(file_config,argv[1]);

   engage_log(argv[2]);
 str_c_to_f(file_config_f,file_config,256);
   pre_processor(file_config_f,&istatus,&numtask,&taskid);
   disengage_log();
 MPI_Finalize();

 exit(istatus);
}
