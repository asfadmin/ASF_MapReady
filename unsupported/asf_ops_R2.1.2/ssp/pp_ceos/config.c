/* SccsId[]= @(#)config.c	2.41 3/24/98 */
static char sccsid_config[]= "@(#)PPconfig.c:2.41";

#include <stdio.h>
#include <syslog.h>


#include "externs.h"
#include "error.h"

int load_config(char *file)
{
	FILE *fp;
	int ireturn,i;
	char dump_s[300];

	if ((fp=fopen(file,"r"))== NULL)
		info_handler(ierr_2, file, "can't open file %s\n",file);

	fscanf(fp,"%s",host);
	fscanf(fp,"%d",&num_node);
	fscanf(fp,"%s",perf_flag);
	fscanf(fp,"%s",processor_savedir);
	fscanf(fp,"%s",control_savedir);
	fscanf(fp,"%s",sourcedir);
	fscanf(fp,"%s",fileodl);
	fscanf(fp,"%s",filestatus);
	fscanf(fp,"%s",filetime);
	fscanf(fp,"%s",filesyserror);
	fscanf(fp,"%d",&ire_sim);
	fscanf(fp,"%d",&burst_start);
	fscanf(fp,"%d",&burst_end);
	for (i=0;i<4;i++)
		fscanf(fp,"%d",&sim_eam_nsp[i]);
	fscanf(fp,"%s",time_file);
	fscanf(fp,"%s",dump_file);
	fscanf(fp,"%s%d",dump_s,&ant_flag);
  	/* overide the ant_flag to be 1 */ 
  	ant_flag = 1; 
	fscanf(fp,"%s",error_file);
	fclose(fp);
	return 0;
}
