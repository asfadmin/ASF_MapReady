/* Alaska SAR Processor (ASP) %W% %E% %U% */
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>
#include <asp_shm.h>

int create_shm( int shmkey, u_int size )
{
	int shmid;

	if ( (shmid = shmget(shmkey, 0,0)) >= 0 ) {
		printf("shared memory key #%d exists\n",shmkey);
        	shmctl(shmid, IPC_RMID, 0L);
	}  
	shmid = shmget(shmkey, size, IPC_CREAT | 0700);
	if ( shmid < 0 ) perror("shmid");
	return( shmid );
}  /* End of create_shm */

void
delete_shm( int shmid, unsigned char *data )
{
        shmdt(data);
        shmctl(shmid, IPC_RMID, 0L);

} /* End of delete_shm */
int asp_get_shm()
{
        /* Initialize shared-memory buffer */
        if( (shmid = create_shm( SHMKEY, SHM_SIZE )) < 0 ){
                printf( "Error in creating flag shared memory buffer\n");
                return(-1);
        }
        shm_data = (unsigned char *) shmat(shmid, 0, 0);
        shm_flags = (SHM_FLAG_PTR) (shm_data + 2*BUFLEN);

} /* End of asp_shm */

void asp_delete_shm()
{
	int shmid;

	if ( (shmid = shmget(SHMKEY, 0,0)) >= 0 ) {
		delete_shm( shmid, shm_data );
	}
} /* End of delete_shm */

int cleanup_shm()
{  
	int shmid;

	if ( (shmid = shmget(SHMKEY, 0,0)) >= 0 ) {
		printf("shared memory key #%d exists\n",SHMKEY);
        	return(shmctl(shmid, IPC_RMID, 0L));
	} return(0);
}

