#ifdef USE_MPI
#define MPI 1

#include "mpi.h"

/*Master commands.*/
extern int first_slave;
extern int num_slaves;
extern MPI_Status mpi_stat;
#define comm MPI_COMM_WORLD

void mpi_init(int *argc,char ***argv);/*Calls MPI_Init, etc.*/
void mpi_end(void);/*Calls MPI_Finalize, etc.*/
void send_slave_command(int slaveNo,int command);

/*Generic commands.*/ 
int get_my_rank(void);

/*Slave commands.*/
void slave_process(int command);
#define SLAVE_PROCESS_BEGIN 1
#define SLAVE_PROCESS_END 999999
#define SLAVE_DIE 1000000

#else
#define MPI 0

#define mpi_init(a,b)
#define mpi_end()
#define get_my_rank() 0

#endif

