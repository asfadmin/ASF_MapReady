/* SccsId[]= @(#)main_ceos.c	2.41 3/24/98 */
#include <stdio.h>
#include "error.h"
static char sccsid_ceos[]=  "@(#)SSP2main_ceos.c:2.41";    

main(argc,argv)
int argc;
char *argv[];
{
char file_config[256];
char file_config_f[256];
int istatus;
int numtask,taskid,rc;
char subsystem_f[60];


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
 str_c_to_f(subsystem_f,argv[2],60);
   ceos_and_output(file_config_f,&istatus,&numtask,&taskid,subsystem_f);
   disengage_log();

 MPI_Finalize();


 exit(istatus);
}
