/*
My_MPI:
	A set of (useful?) wrappers over the MPI
routines that I find useful.

*/
#include <stdio.h>
#include "my_mpi.h"

#if MPI

#define status(mess) printf mess

int first_slave=1;
int num_slaves=-1;
MPI_Status mpi_stat;

void slave_run(void);

/*Init: Calls MPI_Init, etc.
Calls slave_run() on slave processors,
returns for the master processor.
*/
void mpi_init(int *argc,char ***argv)                   
{
        int rank;
        int num;
        MPI_Init(argc,argv);
        rank=get_my_rank();
        status(("Processor %d online.\n",rank));
	if (rank!=0)
        {/*Is slave processor-- call slave_run until done.*/
                slave_run();
                MPI_Finalize();
                exit(0);
        }
	status(("Master running.\n"));
        MPI_Comm_size(MPI_COMM_WORLD,&num);
        num_slaves=num-first_slave;
}

void mpi_end(void)/*Kills slaves, Calls MPI_Finalize, etc.*/
{
	int i;
	status(("Master processor done, and killing slaves.\n"));
	for (i=0;i<num_slaves;i++)
		send_slave_command(i,SLAVE_DIE);
	status(("Master processor about to finalize.\n"));	
	MPI_Finalize();
}
void send_slave_command(int slaveNo,int command)
{
	int slaveRank=first_slave+slaveNo;
	status(("Master sending slave %d command %d\n",slaveRank,command));
	MPI_Send(&command,1,MPI_INT,slaveRank,0,MPI_COMM_WORLD);
}


int get_my_rank(void)
{
        int rank;
        MPI_Comm_rank(MPI_COMM_WORLD,&rank);
        return rank;
}

/*slave_run is the slave processor (!=0)
main event loop.  The slaves wait for a command
from the master, and then execute it.*/
void slave_run(void)        
{
        while (1)
        {
                int command;   
		status(("Slave %d awaiting command.\n",get_my_rank()));
                MPI_Recv(&command,1,MPI_INT,0,0,MPI_COMM_WORLD,&mpi_stat);
                status(("Slave %d recieved command %d\n",get_my_rank(),command));
                if((command>=SLAVE_PROCESS_BEGIN)&&(command<=SLAVE_PROCESS_END))
                        slave_process(command);
                else if (command==SLAVE_DIE)
                        return;
                else {
                        printf("Processor %d recieved unknown command %d from root!\
n",get_my_rank,command);
                        return; 
                }
        }
}

#endif 

